{-# LANGUAGE CPP #-}
-- | Defines a Monad context for calling Alloy

module Capabilities.Alloy (
  MonadAlloy (..),
  getInstances,
  maxBitWidth,
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Language.Alloy.Call (
  AlloyInstance,
  CallAlloyConfig (..),
  SatSolver (..),
  defaultCallAlloyConfig,
  )
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )

class Monad m => MonadAlloy m where
  getInstancesWith :: CallAlloyConfig -> String -> m [AlloyInstance]

instance MonadAlloy m => MonadAlloy (GenericReportT l o m)  where
  getInstancesWith config = lift . getInstancesWith config

getInstances
  :: MonadAlloy m
  => Maybe Integer
  -> Maybe Int
  -> String
  -> m [AlloyInstance]
getInstances maybeMaxInstances maybeTimeout = getInstancesWith
  $ defaultCallAlloyConfig {
    maxInstances = maybeMaxInstances,
#if ALLOY_USE_SAT4J
    satSolver    = SAT4J,
#else
    satSolver    = MiniSat,
#endif
    timeout      = maybeTimeout
    }

maxBitWidth :: Maybe Int
maxBitWidth =
#ifdef MAX_BIT_WIDTH
  Just MAX_BIT_WIDTH
#else
  Nothing
#endif
