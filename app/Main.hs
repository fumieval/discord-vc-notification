{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
module Main where

import RIO
import Prelude.Dot

import Data.Aeson
import Data.Aeson.Types
import Data.HashSet qualified as HS
import Data.List (partition)
import Data.Monoid (Alt(..))
import Data.Time.Clock
import Discord qualified
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS
import Network.WebSockets qualified as WS
import RIO.ByteString.Lazy qualified as BL
import System.Environment
import UnliftIO.Concurrent

type MessageHandler = Object -> Alt Parser (RIO Env ())

data MemberState = MemberState
  { byUser :: HashMap UserId VoiceChannelId
  , byVC :: HashMap VoiceChannelId (HS.HashSet UserId)
  }

updateMemberState :: UserId -> Maybe VoiceChannelId -> MemberState -> MemberState
updateMemberState uid mcid MemberState{..} = MemberState
  { byUser = byUser.alter (const mcid) uid
  , byVC = case mcid of
    Nothing -> case byUser.lookup uid of
      Nothing -> byVC
      Just cid -> byVC.adjust (HS.delete uid) cid
    Just cid -> byVC.insertWith mappend cid (HS.singleton uid)
  }

data Notification = Notification
  { channel :: TextChannelId
  , condition :: (Ordering, Int)
  } deriving Show

data Env = Env
  { hcManager :: HC.Manager
  , wsConn :: WS.Connection
  , botToken :: Text
  , voiceChannelNames :: IORef (HashMap GuildId (HashMap VoiceChannelId Text))
  , watchMap :: IORef (HashMap GuildId (HashMap Text Notification))
  , pendingEvents :: IORef (HashMap UserId (UTCTime, VoiceChannelId, TextChannelId))
  , memberState :: IORef MemberState
  , logFunc :: LogFunc
  }

instance Discord.HasEnv Env where
  getConnection = (.wsConn)
  getToken = (.botToken)
  getManager = (.hcManager)

instance HasLogFunc Env where
  logFuncL = lens (.logFunc) (\s f -> s { logFunc = f })

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

watchList :: Object -> Parser (Maybe [(Text, Notification)])
watchList obj = optional $ do
  topic :: Text <- obj .: "topic"
  tcid <- obj .: "id"
  pure $ do
    str <- topic.lines
    vcnames <- maybeToList
      $ str.stripPrefix "discord-vc-notification:"
      <|> str.stripPrefix "vc-notification:"
    let (conditions, vcnames') = partitionEithers
          $ map (parseThreshold . (.unpack))
          $ vcnames.splitOn " "
    vcname <- vcnames'
    guard $ not $ vcname.null
    pure (vcname, Notification tcid $ case conditions of
      c : _ -> c
      _ -> (GT, 0))

parseThreshold :: String -> Either (Ordering, Int) Text
parseThreshold "first" = Left (EQ, 1)
parseThreshold ('<':str) | Just n <- readMaybe str = Left (LT, n)
parseThreshold ('=':str) | Just n <- readMaybe str = Left (EQ, n)
parseThreshold ('>':str) | Just n <- readMaybe str = Left (GT, n)
parseThreshold s = Right $ pack @Text s

guildCreate :: MessageHandler
guildCreate obj = Alt $ do
  dat <- Discord.event obj "GUILD_CREATE"
  gid@(GuildId rawGid) :: GuildId <- dat .: "id"
  pure $ do
    logInfo $ "Guild " <> display rawGid
    chs :: [Object] <- Discord.api "GET" ["guilds", rawGid, "channels"] Nothing
    let parseOr e = either (const e) id . flip parseEither () . const
    let collect f = parseOr mempty $ fromList . concat . catMaybes <$> traverse f chs
    let wm = collect watchList
    let vcnames = parseOr mempty $ fromList . catMaybes <$> traverse voiceChannelInfo chs
    Env{..} <- ask
    modifyIORef' watchMap $ \m -> m.insert gid wm
    modifyIORef' voiceChannelNames $ \m -> m.insert gid vcnames

channelUpdate :: MessageHandler
channelUpdate obj = Alt $ do
  dat <- Discord.event obj "CHANNEL_UPDATE"
  gid :: GuildId <- obj .: "guild_id"
  watch <- watchList dat
  vcnames <- voiceChannelInfo dat

  pure $ do
    Env{..} <- ask
    case watch of
      Just xs -> modifyIORef' watchMap $ \m -> m.insertWith (<>) gid (fromList xs)
      Nothing -> pure ()
    case vcnames of
      Just (k, v) -> modifyIORef' voiceChannelNames $ \m -> m.insertWith (<>) gid (singleton k v)
      Nothing -> pure ()

postJoined :: [(UserId, VoiceChannelId)] -> TextChannelId -> RIO Env ()
postJoined events (TextChannelId tc) = do
  (_ :: Value) <- Discord.api "POST" ["channels", tc, "messages"]
    $ Just $ object
      [ "content" .= empty @Text
      , "embed" .= object
        [ "description" .= [mconcat ["<@", uid, "> joined <#", vc, ">"] | (UserId uid, VoiceChannelId vc) <- events].unlines
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
      $ \ms -> (updateMemberState uid cid ms, case cid of
        Nothing -> pure $ atomicModifyIORef' pendingEvents
          $ \m -> (m.delete uid, ())
        Just vc -> do
          guard $ case ms.byUser.lookup uid of
            Nothing -> True
            Just vc' -> vc /= vc'
          name <- names.lookup gid >>= \m -> m.lookup vc
          channels <- wm.lookup gid
          Notification target (condition, threshold) <- channels.lookup name

          let count = maybe 0 HS.size $ ms.byVC.lookup vc
          -- check if anyone is in the VC
          guard $ compare (count + 1) threshold == condition
          pure $ atomicModifyIORef' pendingEvents
            $ \m -> (m.insert uid (now, vc, target), ()))
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
    botToken <- pack @Text <$> getEnv "DISCORD_BOT_TOKEN"
    hcManager <- HC.newManager tlsManagerSettings
    voiceChannelNames <- newIORef mempty
    watchMap <- newIORef mempty
    pendingEvents <- newIORef mempty
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
    $ \m -> case partition (\(_, (t0, _, _)) -> now `diffUTCTime` t0 >= delay) $ m.toList of
      (ready, pending) -> (fromList pending, ready)
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
  memberState <- newIORef $ MemberState mempty mempty
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
