{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines transformer instances for capability Diagrams.

module Capabilities.Diagrams.IO.Trans () where

import Capabilities.Diagrams            (MonadDiagrams (lin, renderDiagram))

import Control.Monad.Trans.Except       (ExceptT)
import Control.Monad.Random             (RandT)
import Control.Monad.Trans.Class        (MonadTrans (lift))

instance MonadDiagrams (RandT g IO) where
  lin = lift lin
  renderDiagram = lift . renderDiagram

instance MonadDiagrams (ExceptT e IO) where
  lin = lift lin
  renderDiagram = lift . renderDiagram
