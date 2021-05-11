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
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import System.Environment

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
  return (vcid, name)

watchList :: Object -> Parser (Maybe [(Text, TextChannelId)])
watchList obj = optional $ do
  topic <- obj .: "topic"
  tcid <- obj .: "id"
  return $ do
    str <- T.lines topic
    vcnames <- maybeToList
      $ T.stripPrefix "discord-vc-notification:" str
      <|> T.stripPrefix "vc-notification:" str
    vcname <- T.splitOn " " vcnames
    guard $ not $ T.null vcname
    return (vcname, tcid)

guildCreate :: IO () -> MessageHandler
guildCreate onSuccess obj = Alt $ do
  dat <- event obj "GUILD_CREATE"
  gid@(GuildId rawGid) :: GuildId <- dat .: "id"
  return $ do
    chs :: [Object] <- discordApi "GET" ["guilds", rawGid, "channels"] Nothing
    let parseOr e = either (const e) id . flip parseEither () . const
    let collect f = parseOr mempty $ HM.fromList . concat . catMaybes <$> traverse f chs
    let wm = collect watchList
    let vcnames = parseOr mempty $ HM.fromList . catMaybes <$> traverse voiceChannelInfo chs
    Env{..} <- ask
    modifyIORef' watchMap $ HM.insert gid wm
    modifyIORef' voiceChannelNames $ HM.insert gid vcnames
    liftIO onSuccess

channelUpdate :: MessageHandler
channelUpdate obj = Alt $ do
  dat <- event obj "CHANNEL_UPDATE"
  gid :: GuildId <- obj .: "guild_id"
  watch <- watchList dat
  vcnames <- voiceChannelInfo dat

  return $ do
    Env{..} <- ask
    case watch of
      Just xs -> modifyIORef' watchMap $ HM.insertWith HM.union gid (HM.fromList xs)
      Nothing -> pure ()
    case vcnames of
      Just (k, v) -> modifyIORef' voiceChannelNames $ HM.insertWith HM.union gid (HM.singleton k v)
      Nothing -> pure ()

postJoined :: [(UserId, VoiceChannelId)] -> TextChannelId -> RIO Env ()
postJoined events (TextChannelId tc) = do
  (_ :: Value) <- discordApi "POST" ["channels", tc, "messages"]
    $ Just $ object
      [ "content" .= T.empty
      , "embed" .= object
        [ "description" .= T.unlines [T.concat ["<@", uid, "> joined <#", vc, ">"] | (UserId uid, VoiceChannelId vc) <- events]
        ]
      , "allowed_mentions" .= object ["parse" .= ([] :: [Value])]
      ]
  return ()

voiceChannelJoin :: MessageHandler
voiceChannelJoin obj = Alt $ do
  dat <- event obj "VOICE_STATE_UPDATE"
  cid <- dat .:? "channel_id"
  uid <- dat .: "user_id"
  gid <- dat .: "guild_id"
  return $ do
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

opcode :: FromJSON a => Object -> Int -> Parser a
opcode obj i = do
  op <- obj .: "op"
  if op == i
    then obj .: "d"
    else fail $ "Unexpected opcode: " ++ show op

event :: Object -> Text -> Parser Object
event obj name = do
  d <- opcode obj 0
  t <- obj .: "t"
  guard $ name == t
  return d

ackHeartbeat :: MessageHandler
ackHeartbeat obj = Alt $ do
  _ <- opcode obj 11 :: Parser Value
  return (pure ())

hello :: MessageHandler
hello obj = Alt $ do
  dat <- opcode obj 10
  period <- dat .: "heartbeat_interval"
  return $ do
    _ <- forkIO $ sendHeartbeat period
    identify

sendHeartbeat :: Int -> RIO Env ()
sendHeartbeat period = forever $ do
  send $ object ["op" .= (1 :: Int), "d" .= (251 :: Int)]
  threadDelay $ 1000 * period

identify :: RIO Env ()
identify = do
  Env{..} <- ask
  send $ object
    [ "op" .= (2 :: Int)
    , "d" .= object
      [ "token" .= botToken
      , "properties" .= object
        [ "$os" .= T.pack "linux"
        , "$browser" .= T.pack "discord-vc-notification"
        , "$device" .= T.pack "discord-vc-notification"
        ]
      , "compress" .= False
      , "large_threshold" .= (250 :: Int)
      , "shard" .= [0 :: Int, 1]
      , "presence" .= object
        [ "game" .= Null
        , "status" .= T.pack "online"
        , "since" .= Null
        , "afk" .= False
        ]
      ]
    ]

ignoreEvent :: MessageHandler
ignoreEvent obj = Alt $ do
  (_ :: Value) <- opcode obj 0
  return $ pure ()

combined :: IO () -> MessageHandler
combined onSuccess = mconcat
  [ ackHeartbeat
  , hello
  , guildCreate onSuccess
  , channelUpdate
  , voiceChannelJoin
  , ignoreEvent
  ]

send :: Value -> RIO Env ()
send v = ask >>= \Env{..} -> liftIO $ WS.sendTextData wsConn $ encode v

discordApi :: FromJSON a => Method -> [Text] -> Maybe Value -> RIO Env a
discordApi m ps obj = ask >>= \Env{..} -> do
  initialRequest <- liftIO $ HC.parseRequest "https://discordapp.com/"
  resp <- liftIO $ HC.httpLbs initialRequest
    { HC.method = m
    , HC.path = T.encodeUtf8 $ T.intercalate "/" $ "/api" : ps
    , HC.requestBody = maybe mempty (HC.RequestBodyLBS . encode) obj
    , HC.requestHeaders =
      [ ("Authorization", "Bot " <> T.encodeUtf8 botToken)
      , ("User-Agent", "discord-vc-notification")
      , ("Content-Type", "application/json")
      ]
    }
    hcManager
  case decode $ HC.responseBody resp of
    Nothing -> error $ "Malformed response: " ++ show (HC.responseBody resp)
    Just a -> return a

start :: LogFunc -> IORef MemberState -> IO () -> IO ()
start logFunc memberState onSuccess = WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"
  $ \wsConn -> do
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
          Just obj -> case parse (getAlt . combined onSuccess) obj of
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
      start logFunc memberState (writeIORef retryInterval minInterval)
        `catch` \e -> do
          runRIO logFunc $ logError $ displayShow (e :: SomeException)
      t <- readIORef retryInterval
      runRIO logFunc $ logWarn $ "Restarting in " <> displayShow t
      threadDelay $ floor $ t * 1000000
      modifyIORef retryInterval (*1.5)
  where
    minInterval :: Double
    minInterval = 30
