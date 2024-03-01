module Core.Err
  ( Error (..),
    Exception,
    IOException,
    try,
    catch,
    throw,
    throwIO,
    bracket,
    bracketOnError,
  )
where

import Control.Exception
import Data.Typeable (Typeable)

newtype Error = Error String deriving (Show, Typeable)

instance Exception Error
