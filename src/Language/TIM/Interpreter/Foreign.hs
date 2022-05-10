module Language.TIM.Interpreter.Foreign where

import Foreign.C

----------------------------------------
-- RTS initialization

foreign import ccall "new_rts_stdout"   c_new_rts_stdout   :: CString -> IO ()
foreign import ccall "close_rts_stdout" c_close_rts_stdout :: IO ()

withRTSStdout :: FilePath -> IO a -> IO a
withRTSStdout path io = do
  c_path <- newCString path
  c_new_rts_stdout c_path
  a <- io
  c_close_rts_stdout
  return a