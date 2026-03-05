{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines an IO based GenericReportT instance for capability Cache.

module Capabilities.Cache.IO () where

import qualified Data.ByteString        as BS (readFile, writeFile)

import Capabilities.Cache               (MonadFileCache (..))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import System.Directory                 (doesFileExist)

instance MonadFileCache (GenericReportT l o IO)  where
  appendCollisionFile f = lift . appendFile f
  doesCacheExist = lift . doesFileExist
  readShowFile = lift . BS.readFile
  writeShowFile f = lift . BS.writeFile f
