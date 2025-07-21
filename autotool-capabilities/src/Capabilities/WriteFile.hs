-- | Defines a monad context for writing a file to disc.

module Capabilities.WriteFile (
  MonadWriteFile (..),
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.ByteString                  (ByteString)

class Monad m => MonadWriteFile m where
  writeToFile :: FilePath -> ByteString -> m ()

instance MonadWriteFile m => MonadWriteFile (GenericReportT l o m)  where
  writeToFile path = lift . writeToFile path
