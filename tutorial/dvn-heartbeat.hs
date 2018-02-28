{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import RIO
import UnliftIO.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Network.WebSockets as WS
import qualified Wuss as WS

data Env = Env
  { wsConn :: WS.Connection
  , logFunc :: LogFunc
  }

instance HasLogFunc Env where
  logFuncL = to logFunc

send :: Value -> RIO Env ()
send v = ask >>= \Env{..} -> liftIO $ WS.sendTextData wsConn $ encode v

sendHeartbeat :: Int -> RIO Env ()
sendHeartbeat period = forever $ do
  send $ object ["op" .= (1 :: Int), "d" .= (251 :: Int)]
  liftIO $ threadDelay $ 1000 * period

hello :: Object -> Parser (RIO Env ())
hello obj = do
  op <- obj .: "op"
  guard $ op == (10 :: Int)
  dat <- obj .: "d"
  interval <- dat .: "heartbeat_interval"
  return $ void $ forkIO $ sendHeartbeat interval

main :: IO ()
main = WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"
  $ \wsConn -> do
    logOpts <- mkLogOptions stderr True
    withStickyLogger logOpts $ \logFunc -> forever $ do
      bs <- WS.receiveData wsConn
      obj <- case decode bs of
        Nothing -> fail "Failed to parse a JSON object"
        Just a -> pure a
      runRIO Env{..} $ case parse hello obj of
        Success m -> m
        Error _ -> logWarn $ "Unhandled: " <> displayBytesUtf8 (toStrictBytes bs)
