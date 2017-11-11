import qualified Data.ByteString.Lazy as B
import qualified NintyCompress as Ninty

main::IO()
main = B.interact (B.pack . Ninty.decompress . B.unpack)
