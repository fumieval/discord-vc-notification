import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Network.WebSockets as WS
import qualified Wuss as WS

main = WS.runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"
  $ \wsConn -> forever $ WS.receiveData wsConn >>= B.putStrLn
