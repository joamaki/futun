{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module TunTap (openTun) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
--import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C.String

import System.Posix.Types

foreign import ccall unsafe "tuntap.h open_tun"
  c_open_tun :: Ptr CChar -> IO CInt

openTun :: IO (String, Fd)
openTun = do
  arr <- mallocBytes 256 :: IO (Ptr CChar)
  fd <- c_open_tun arr
  str <- peekCString arr
  free arr
  return $ (str, Fd fd)
