{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines an IO based GenericReportT instance for capability Cache.

module Capabilities.Cache.IO () where

import qualified Data.ByteString.Lazy   as LBS (fromStrict)
import qualified Data.ByteString.UTF8   as BS (fromString)
import qualified Data.ByteString        as BS (readFile, writeFile)

import Capabilities.Cache               (MonadFileCache (..))
import Control.Monad                    (when)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.Digest.Pure.SHA             (sha256, showDigest)
import System.Directory                 (doesFileExist)

instance MonadFileCache (GenericReportT l o IO)  where
  cacheFile path ext name what how = file <$
      do
        let create = how what >>= (lift . BS.writeFile file) >>
                     lift (BS.writeFile whatFile what')
        isFile <- lift $ doesFileExist file
        if isFile
          then do
            f <- lift $ BS.readFile whatFile
            when (f /= what') $ do
              lift $ appendFile (path ++ "busted.txt") whatId
              create
          else create
    where
      what' = BS.fromString $ show what
      whatId = path ++ name ++ showDigest (sha256 $ LBS.fromStrict what')
      whatFile = whatId ++ ".hs"
      file = whatId ++ ext
