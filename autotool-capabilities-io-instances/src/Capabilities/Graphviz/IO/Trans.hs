{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines transformer instances for capability Graphviz.

module Capabilities.Graphviz.IO.Trans () where

import Capabilities.Graphviz            (MonadGraphviz (..))

import Control.Monad.Random             (RandT)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.Monad.Trans.Except       (ExceptT)

instance MonadGraphviz (RandT g IO)  where
  errorWithoutGraphviz = lift errorWithoutGraphviz
  layoutGraph command = lift . layoutGraph command
  layoutGraph' params command = lift . layoutGraph' params command

instance MonadGraphviz (ExceptT e IO) where
  errorWithoutGraphviz = lift errorWithoutGraphviz
  layoutGraph command = lift . layoutGraph command
  layoutGraph' params command = lift . layoutGraph' params command
