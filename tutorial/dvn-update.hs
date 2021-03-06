{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import RIO
import UnliftIO.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Monoid (Alt(..))
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import System.Environment

data Env = Env
  { wsConn :: WS.Connection
  , botToken :: Text
  , logFunc :: LogFunc
  , watchMap :: IORef (HM.HashMap Text Text)
  , memberState :: IORef (HM.HashMap Text Text)
  }

instance HasLogFunc Env where
  logFuncL = lens logFunc (\s f -> s { logFunc = f })

send :: Value -> RIO Env ()
send v = ask >>= \Env{..} -> liftIO $ WS.sendTextData wsConn $ encode v

sendHeartbeat :: Int -> RIO Env ()
sendHeartbeat period = forever $ do
  send $ object ["op" .= (1 :: Int), "d" .= (251 :: Int)]
  liftIO $ threadDelay $ 1000 * period

type MessageHandler = Object -> Alt Parser (RIO Env ())

hello :: MessageHandler
hello obj = Alt $ do
  op <- obj .: "op"
  guard $ op == (10 :: Int)
  dat <- obj .: "d"
  interval <- dat .: "heartbeat_interval"
  return $ do
    _ <- forkIO $ sendHeartbeat interval
    identify

identify :: RIO Env ()
identify = do
  Env{..} <- ask
  send $ object
    [ "op" .= (2 :: Int)
    , "d" .= object
      [ "token" .= botToken
      , "properties" .= object
        [ "$os" .= ("linux" :: Text)
        , "$browser" .= ("discord-vc-notification" :: Text)
        , "$device" .= ("discord-vc-notification" :: Text)
        ]
      , "compress" .= False
      , "large_threshold" .= (250 :: Int)
      , "shard" .= [0 :: Int, 1]
      , "presence" .= object
        [ "game" .= Null
        , "status" .= ("online" :: Text)
        , "since" .= Null
        , "afk" .= False
        ]
      ]
    ]

guildCreate :: MessageHandler
guildCreate obj = Alt $ do
  t <- obj .: "t"
  guard $ t == ("GUILD_CREATE" :: Text)
  dat <- obj .: "d"
  chs <- dat .: "channels"
  wm <- HM.fromList . concat <$> traverse watchList (chs :: [Object])
  return $ do
    Env{..} <- ask
    writeIORef watchMap wm
    logDebug $ "watchlist: " <> displayShow wm

watchList :: Object -> Parser [(Text, Text)]
watchList obj = do
  topic <- obj .: "topic"
  tcid <- obj .: "id"
  return $ do
    str <- T.lines topic
    vcids <- maybeToList $ T.stripPrefix "discord-vc-notification:" str
    vcid <- T.splitOn " " vcids
    guard $ not $ T.null vcid
    return (vcid, tcid)
  <|> pure []

voiceChannelJoin :: MessageHandler
voiceChannelJoin obj = Alt $ do
  t <- obj .: "t"
  guard $ t == ("VOICE_STATE_UPDATE" :: Text)
  dat <- obj .: "d"
  cid <- dat .:? "channel_id"
  uid <- dat .: "user_id"
  return $ do
    Env{..} <- ask
    wm <- readIORef watchMap
    joined <- atomicModifyIORef memberState
      $ \ms -> (HM.alter (const cid) uid ms, do
        guard $ not $ HM.member uid ms
        vc <- cid
        tc <- HM.lookup vc wm
        return $ postJoined uid vc tc)
    sequence_ joined

postJoined :: Text -> Text -> Text -> RIO Env ()
postJoined uid vc tc = logInfo
  $ mconcat [display tc, ": ", display uid, " joined ", display vc]

combined :: MessageHandler
combined = mconcat
  [ hello
  , guildCreate
  , voiceChannelJoin
  ]

main :: IO ()
main = WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"
  $ \wsConn -> do
    botToken <- fromString <$> getEnv "DISCORD_BOT_TOKEN"
    logOpts <- logOptionsHandle stderr True
    watchMap <- newIORef HM.empty
    memberState <- newIORef HM.empty
    withLogFunc logOpts $ \logFunc -> forever $ do
      bs <- WS.receiveData wsConn
      obj <- case decode bs of
        Nothing -> fail "Failed to parse a JSON object"
        Just a -> pure a
      runRIO Env{..} $ case parse (getAlt . combined) obj of
        Success m -> m
        Error _ -> logWarn $ "Unhandled: " <> displayBytesUtf8 (toStrictBytes bs)
