module Core.Common(PrintIO, println) where

import Control.Monad.IO.Class

class MonadIO m => PrintIO m where
  println :: String -> m ()

instance PrintIO IO where
    println = putStrLn
