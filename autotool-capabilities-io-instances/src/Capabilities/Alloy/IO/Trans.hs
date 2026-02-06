{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines transformer instances for capability Alloy.

module Capabilities.Alloy.IO.Trans () where

import Capabilities.Alloy               (MonadAlloy (..))

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Random             (RandT)

instance MonadAlloy (RandT g IO) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadAlloy (ExceptT e IO) where
  getInstancesWith config = lift . getInstancesWith config
