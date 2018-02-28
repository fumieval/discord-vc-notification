{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import RIO
import UnliftIO.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import System.Environment

data Env = Env
  { wsConn :: WS.Connection
  , hcManager :: HC.Manager
  , botToken :: Text
  , logFunc :: LogFunc
  , watchMap :: IORef (HM.HashMap Text Text)
  , memberState :: IORef (HM.HashMap Text Text)
  }

instance HasLogFunc Env where
  logFuncL = to logFunc

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

postJoined :: Text -- user id
  -> Text -- voice channel id
  -> Text -- text channel id
  -> RIO Env ()
postJoined uid vc tc = do
  now <- liftIO getCurrentTime
  uInfo <- discordApi "GET" ["users", uid] Nothing
  author <- either fail pure $ flip parseEither uInfo $ const $ do
    name <- uInfo .: "username"
    avatar <- uInfo .: "avatar"
    return $ object
      [ "name" .= (name :: Text)
      , "icon_url" .= T.intercalate "/"
        ["https://cdn.discordapp.com", "avatars", uid, avatar <> ".png?size=256"]
      ]
  void $ discordApi "POST" ["channels", tc, "messages"]
    $ Just $ object
      [ "content" .= T.empty
      , "embed" .= object
        [ "description" .= T.concat ["Joined <#", vc, ">"]
        , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
        , "author" .= author
        ]
      ]

discordApi :: Method -> [Text] -> Maybe Value -> RIO Env Object
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
    Nothing -> fail $ "Malformed response: " ++ show (HC.responseBody resp)
    Just a -> return a

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
    logOpts <- mkLogOptions stderr True
    watchMap <- newIORef HM.empty
    memberState <- newIORef HM.empty
    hcManager <- HC.newManager tlsManagerSettings
    withStickyLogger logOpts $ \logFunc -> forever $ do
      bs <- WS.receiveData wsConn
      obj <- case decode bs of
        Nothing -> fail "Failed to parse a JSON object"
        Just a -> pure a
      runRIO Env{..} $ case parse (getAlt . combined) obj of
        Success m -> m
        Error _ -> logWarn $ "Unhandled: " <> displayBytesUtf8 (toStrictBytes bs)
