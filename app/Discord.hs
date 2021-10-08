{-# LANGUAGE OverloadedStrings #-}
module Discord where

import RIO
import Data.Aeson
import Data.Aeson.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import qualified Wuss as WS

class HasEnv a where
  getToken :: a -> Text
  getConnection :: a -> WS.Connection
  getManager :: a -> HC.Manager

api :: (HasEnv env, FromJSON a) => Method -> [Text] -> Maybe Value -> RIO env a
api m ps obj = ask >>= \env -> do
  initialRequest <- liftIO $ HC.parseRequest "https://discordapp.com/"
  resp <- liftIO $ HC.httpLbs initialRequest
    { HC.method = m
    , HC.path = T.encodeUtf8 $ T.intercalate "/" $ "/api" : ps
    , HC.requestBody = maybe mempty (HC.RequestBodyLBS . encode) obj
    , HC.requestHeaders =
      [ ("Authorization", "Bot " <> T.encodeUtf8 (getToken env))
      , ("User-Agent", "discord-vc-notification")
      , ("Content-Type", "application/json")
      ]
    }
    (getManager env)
  case decode $ HC.responseBody resp of
    Nothing -> error $ "Malformed response: " ++ show (HC.responseBody resp)
    Just a -> return a

withGateway :: WS.ClientApp a -> IO a
withGateway = WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"

identify :: HasEnv env => RIO env ()
identify = do
  token <- asks getToken
  send $ object
    [ "op" .= (2 :: Int)
    , "d" .= object
      [ "token" .= token
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

send :: HasEnv env => Value -> RIO env ()
send v = ask >>= \env -> liftIO $ WS.sendTextData (getConnection env) $ encode v

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

sendHeartbeat :: HasEnv env => Int -> RIO env ()
sendHeartbeat period = forever $ do
  send $ object ["op" .= (1 :: Int), "d" .= (251 :: Int)]
  threadDelay $ 1000 * period
