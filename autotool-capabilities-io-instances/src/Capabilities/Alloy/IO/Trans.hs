{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines transformer instances for capability Alloy.

module Capabilities.Alloy.IO.Trans () where

import Capabilities.Alloy               (MonadAlloy (..))
import Capabilities.Alloy.IO            ()

import Control.Monad.Random             (RandT)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)

instance MonadAlloy (RandT g IO) where
  getInstancesWith config = lift . getInstancesWith config

instance MonadAlloy (ExceptT e IO) where
  getInstancesWith config = lift . getInstancesWith config
