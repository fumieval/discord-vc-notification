{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RIO
import UnliftIO.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Data.Monoid (Alt(..))
import Data.Time.Clock
import qualified RIO.ByteString.Lazy as BL
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Environment
import qualified Discord

type MessageHandler = Object -> Alt Parser (RIO Env ())

type MemberState = HM.HashMap UserId VoiceChannelId

data Env = Env
  { hcManager :: HC.Manager
  , wsConn :: WS.Connection
  , botToken :: Text
  , voiceChannelNames :: IORef (HM.HashMap GuildId (HM.HashMap VoiceChannelId Text))
  , watchMap :: IORef (HM.HashMap GuildId (HM.HashMap Text TextChannelId))
  , pendingEvents :: IORef (HM.HashMap UserId (UTCTime, VoiceChannelId, TextChannelId))
  , memberState :: IORef MemberState
  , logFunc :: LogFunc
  }

instance Discord.HasEnv Env where
  getConnection = wsConn
  getToken = botToken
  getManager = hcManager

instance HasLogFunc Env where
  logFuncL = lens logFunc (\s f -> s { logFunc = f })

newtype VoiceChannelId = VoiceChannelId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype TextChannelId = TextChannelId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype UserId = UserId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype GuildId = GuildId Text deriving (Show, Eq, Ord, Hashable, FromJSON)

voiceChannelInfo :: Object -> Parser (Maybe (VoiceChannelId, Text))
voiceChannelInfo obj = optional $ do
  ty <- obj .: "type"
  guard $ ty == (2 :: Int) -- Voice channel
  vcid <- obj .: "id"
  name <- obj .: "name"
  pure (vcid, name)

watchList :: Object -> Parser (Maybe [(Text, TextChannelId)])
watchList obj = optional $ do
  topic <- obj .: "topic"
  tcid <- obj .: "id"
  pure $ do
    str <- T.lines topic
    vcnames <- maybeToList
      $ T.stripPrefix "discord-vc-notification:" str
      <|> T.stripPrefix "vc-notification:" str
    vcname <- T.splitOn " " vcnames
    guard $ not $ T.null vcname
    pure  (vcname, tcid)

guildCreate :: MessageHandler
guildCreate obj = Alt $ do
  dat <- Discord.event obj "GUILD_CREATE"
  gid@(GuildId rawGid) :: GuildId <- dat .: "id"
  pure $ do
    chs :: [Object] <- Discord.api "GET" ["guilds", rawGid, "channels"] Nothing
    let parseOr e = either (const e) id . flip parseEither () . const
    let collect f = parseOr mempty $ HM.fromList . concat . catMaybes <$> traverse f chs
    let wm = collect watchList
    let vcnames = parseOr mempty $ HM.fromList . catMaybes <$> traverse voiceChannelInfo chs
    Env{..} <- ask
    modifyIORef' watchMap $ HM.insert gid wm
    modifyIORef' voiceChannelNames $ HM.insert gid vcnames

channelUpdate :: MessageHandler
channelUpdate obj = Alt $ do
  dat <- Discord.event obj "CHANNEL_UPDATE"
  gid :: GuildId <- obj .: "guild_id"
  watch <- watchList dat
  vcnames <- voiceChannelInfo dat

  pure $ do
    Env{..} <- ask
    case watch of
      Just xs -> modifyIORef' watchMap $ HM.insertWith HM.union gid (HM.fromList xs)
      Nothing -> pure ()
    case vcnames of
      Just (k, v) -> modifyIORef' voiceChannelNames $ HM.insertWith HM.union gid (HM.singleton k v)
      Nothing -> pure ()

postJoined :: [(UserId, VoiceChannelId)] -> TextChannelId -> RIO Env ()
postJoined events (TextChannelId tc) = do
  (_ :: Value) <- Discord.api "POST" ["channels", tc, "messages"]
    $ Just $ object
      [ "content" .= T.empty
      , "embed" .= object
        [ "description" .= T.unlines [T.concat ["<@", uid, "> joined <#", vc, ">"] | (UserId uid, VoiceChannelId vc) <- events]
        ]
      , "allowed_mentions" .= object ["parse" .= ([] :: [Value])]
      ]
  pure ()

voiceChannelJoin :: MessageHandler
voiceChannelJoin obj = Alt $ do
  dat <- Discord.event obj "VOICE_STATE_UPDATE"
  cid <- dat .:? "channel_id"
  uid <- dat .: "user_id"
  gid <- dat .: "guild_id"
  pure $ do
    Env{..} <- ask
    wm <- readIORef watchMap
    names <- readIORef voiceChannelNames
    now <- liftIO getCurrentTime
    joined <- atomicModifyIORef memberState
      $ \ms -> (HM.alter (const cid) uid ms, case cid of
        Nothing -> pure $ atomicModifyIORef' pendingEvents
          $ \m -> (HM.delete uid m, ())
        Just vc -> do
          guard $ case HM.lookup uid ms of
            Nothing -> True
            Just vc' -> vc /= vc'
          name <- HM.lookup gid names >>= HM.lookup vc
          channels <- HM.lookup gid wm
          target <- HM.lookup name channels
          pure $ atomicModifyIORef' pendingEvents
            $ \m -> (HM.insert uid (now, vc, target) m, ()))
    sequence_ joined

ackHeartbeat :: MessageHandler
ackHeartbeat obj = Alt $ do
  _ <- Discord.opcode obj 11 :: Parser Value
  pure (pure ())

hello :: MessageHandler
hello obj = Alt $ do
  dat <- Discord.opcode obj 10
  period <- dat .: "heartbeat_interval"
  pure $ do
    _ <- forkIO $ Discord.sendHeartbeat period
    Discord.identify

ignoreEvent :: MessageHandler
ignoreEvent obj = Alt $ do
  (_ :: Value) <- Discord.opcode obj 0
  pure $ pure ()

combined :: MessageHandler
combined = mconcat
  [ ackHeartbeat
  , hello
  , guildCreate
  , channelUpdate
  , voiceChannelJoin
  , ignoreEvent
  ]

start :: LogFunc -> IORef MemberState -> IO ()
start logFunc memberState = Discord.withGateway $ \wsConn -> do
    botToken <- T.pack <$> getEnv "DISCORD_BOT_TOKEN"
    hcManager <- HC.newManager tlsManagerSettings
    voiceChannelNames <- newIORef HM.empty
    watchMap <- newIORef HM.empty
    pendingEvents <- newIORef HM.empty
    runRIO Env{..}
      $ bracket (forkIO resolvePending) killThread $ \_ -> forever $ do
        bs <- liftIO $ WS.receiveData wsConn
        case decode bs of
          Nothing -> logError $ "Malformed message from gateway: " <> displayBytesUtf8 (BL.toStrict bs)
          Just obj -> case parse (getAlt . combined) obj of
            Success m -> m
            Error _ -> logWarn $ "Unhandled: " <> displayShow bs

resolvePending :: RIO Env ()
resolvePending = forever $ do
  Env{..} <- ask
  now <- liftIO getCurrentTime
  ready <- atomicModifyIORef' pendingEvents
    $ \m -> case partition (\(_, (t0, _, _)) -> now `diffUTCTime` t0 >= delay) $ HM.toList m of
      (ready, pending) -> (HM.fromList pending, ready)
  forM_ ready $ \(uid, (_, vc, target)) -> postJoined [(uid, vc)] target
    `catch` \(e :: SomeException) -> logError $ displayShow e
  threadDelay 1000000
  where
    -- delay this number of seconds before posting a message, preventing a notification from an accidental click etc
    delay = 10

main :: IO ()
main = do
  retryInterval <- newIORef minInterval
  logOpts <- logOptionsHandle stderr True
  memberState <- newIORef HM.empty
  forever $ do
    withLogFunc logOpts $ \logFunc -> do
      runRIO logFunc $ logInfo "Ready"
      t0 <- getMonotonicTime
      start logFunc memberState
        `catch` \e -> do
          runRIO logFunc $ logError $ displayShow (e :: SomeException)
      t1 <- getMonotonicTime
      t <- readIORef retryInterval
      runRIO logFunc $ logWarn $ "Restarting in " <> displayShow t
      threadDelay $ floor $ t * 1000000
      if t1 - t0 > 180
        then writeIORef retryInterval minInterval
        else modifyIORef retryInterval (*1.5)
  where
    minInterval :: Double
    minInterval = 30
