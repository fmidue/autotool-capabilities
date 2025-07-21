{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability WriteFile.

module Capabilities.WriteFile.IO () where

import qualified Data.ByteString        as BS (writeFile)

import Capabilities.WriteFile           (MonadWriteFile (..))


instance MonadWriteFile IO where
  writeToFile = BS.writeFile
