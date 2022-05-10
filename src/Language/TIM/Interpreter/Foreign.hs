module Language.TIM.Interpreter.Foreign where

import Foreign.C

----------------------------------------
-- RTS initialization

foreign import ccall "open_rts_stdin"   c_open_rts_stdin   :: CString -> IO ()
foreign import ccall "open_rts_stdout"  c_open_rts_stdout  :: CString -> IO ()

foreign import ccall "close_rts_stdin"  c_close_rts_stdin  :: IO ()
foreign import ccall "close_rts_stdout" c_close_rts_stdout :: IO ()

withRTSIO :: FilePath -> FilePath -> IO a -> IO a
withRTSIO stdin stdout io = do
  c_stdin  <- newCString stdin
  c_stdout <- newCString stdout
  c_open_rts_stdin c_stdin
  c_open_rts_stdout c_stdout
  a <- io
  c_close_rts_stdin
  c_close_rts_stdout
  return a