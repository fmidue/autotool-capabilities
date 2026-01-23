{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines the IO instance for capability Alloy

module Capabilities.Alloy.IO () where

import Capabilities.Alloy               (MonadAlloy (..))

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Trans.Random       (RandT)

import qualified Language.Alloy.Call              as Alloy (
  getInstancesWith,
  )

instance MonadAlloy IO where
  getInstancesWith = Alloy.getInstancesWith

instance MonadAlloy (RandT g IO) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadAlloy (ExceptT e IO) where
  getInstancesWith config = lift . getInstancesWith config
