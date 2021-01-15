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
import Data.Monoid (Alt(..))
import Data.Time.Clock
import Data.Time.Format
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
  , voiceChannelNames :: IORef (HM.HashMap VoiceChannelId (GuildId, Text))
  , watchMap :: IORef (HM.HashMap (GuildId, Text) TextChannelId)
  , memberState :: IORef MemberState
  , logFunc :: LogFunc
  }

instance HasLogFunc Env where
  logFuncL = lens logFunc (\s f -> s { logFunc = f })

newtype VoiceChannelId = VoiceChannelId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype TextChannelId = TextChannelId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype UserId = UserId Text deriving (Show, Eq, Ord, Hashable, FromJSON)
newtype GuildId = GuildId Text deriving (Show, Eq, Ord, Hashable, FromJSON)

voiceChannelInfo :: Object -> Parser [(VoiceChannelId, (GuildId, Text))]
voiceChannelInfo obj = do
  ty <- obj .: "type"
  guard $ ty == (2 :: Int) -- Voice channel
  vcid <- obj .: "id"
  gid <- obj .: "guild_id"
  name <- obj .: "name"
  return [(vcid, (gid, name))]
  <|> pure []

watchList :: Object -> Parser [((GuildId, Text), TextChannelId)]
watchList obj = do
  topic <- obj .: "topic"
  tcid <- obj .: "id"
  gid <- obj .: "guild_id"
  return $ do
    str <- T.lines topic
    vcnames <- maybeToList
      $ T.stripPrefix "discord-vc-notification:" str
      <|> T.stripPrefix "vc-notification:" str
    vcname <- T.splitOn " " vcnames
    guard $ not $ T.null vcname
    return ((gid, vcname), tcid)
  <|> pure []

guildCreate :: IO () -> MessageHandler
guildCreate onSuccess obj = Alt $ do
  dat <- event obj "GUILD_CREATE"
  gid <- dat .: "id"
  return $ do
    chs :: [Object] <- discordApi "GET" ["guilds", gid, "channels"] Nothing
    let parseOr e = either (const e) id . flip parseEither () . const
    let collect f = parseOr mempty $ HM.fromList . concat <$> traverse f chs
    let wm = collect watchList
    let vcnames = collect voiceChannelInfo
    Env{..} <- ask
    modifyIORef watchMap (`HM.union`wm)
    modifyIORef voiceChannelNames (`HM.union`vcnames)
    logInfo $ displayShow wm
    liftIO onSuccess

channelUpdate :: MessageHandler
channelUpdate obj = Alt $ do
  dat <- event obj "CHANNEL_UPDATE"
  wm <- HM.fromList <$> watchList dat
  vcnames <- HM.fromList <$> voiceChannelInfo dat

  return $ do
    Env{..} <- ask
    modifyIORef watchMap (`HM.union` wm)
    modifyIORef voiceChannelNames (`HM.union` vcnames)
    logInfo $ displayShow wm

postJoined :: UserId -> VoiceChannelId -> TextChannelId -> RIO Env ()
postJoined (UserId uid) (VoiceChannelId vc) (TextChannelId tc) = do
  now <- liftIO getCurrentTime
  uInfo <- discordApi "GET" ["users", uid] Nothing
  let printError e = do
        logError $ fromString e
        return Null
  author <- either printError pure $ flip parseEither uInfo $ const $ do
    name <- uInfo .: "username"
    avatar <- uInfo .: "avatar"
    return $ object
      [ "name" .= (name :: Text)
      , "icon_url" .= T.intercalate "/"
        ["https://cdn.discordapp.com", "avatars", uid, avatar <> ".png?size=256"]
      ]

  (_ :: Value) <- discordApi "POST" ["channels", tc, "messages"]
    $ Just $ object
      [ "content" .= T.empty
      , "embed" .= object
        [ "description" .= T.concat ["Joined <#", vc, ">"]
        , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
        , "author" .= author
        ]
      ]
  return ()

voiceChannelJoin :: MessageHandler
voiceChannelJoin obj = Alt $ do
  dat <- event obj "VOICE_STATE_UPDATE"
  cid <- dat .:? "channel_id"
  uid <- dat .: "user_id"
  return $ do
    Env{..} <- ask
    wm <- readIORef watchMap
    names <- readIORef voiceChannelNames
    joined <- atomicModifyIORef memberState
      $ \ms -> (HM.alter (const cid) uid ms, do
        vc <- cid
        guard $ not $ HM.member uid ms
        name <- HM.lookup vc names
        postJoined uid vc <$> HM.lookup name wm)
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
    forever $ do
      bs <- WS.receiveData wsConn
      runRIO Env{..} $ case decode bs of
        Nothing -> logError $ "Malformed message from gateway: " <> displayBytesUtf8 (BL.toStrict bs)
        Just obj -> case parse (getAlt . combined onSuccess) obj of
          Success m -> m
          Error _ -> logWarn $ "Unhandled: " <> displayShow bs

main :: IO ()
main = do
  retryInterval <- newIORef minInterval
  logOpts <- logOptionsHandle stderr True
  memberState <- newIORef HM.empty
  forever $ do
    withLogFunc logOpts $ \logFunc -> do
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
