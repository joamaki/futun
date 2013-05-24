{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module TunTap (openTun) where

import Foreign.C.String
import Foreign.C.Types
import System.Posix.Types

foreign import ccall unsafe "tuntap.h open_tun"
  c_open_tun :: CString -> IO CInt

openTun :: String -> IO (Maybe Fd)
openTun name = do
  fd <- withCString name $ \cname -> c_open_tun cname
  return $ if fd < 0
    then Nothing
    else Just (Fd fd)
