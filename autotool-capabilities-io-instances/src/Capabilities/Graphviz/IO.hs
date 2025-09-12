{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability Graphviz.

module Capabilities.Graphviz.IO () where

import qualified Diagrams.TwoD.GraphViz           as GV (
  layoutGraph,
  layoutGraph',
  )

import Capabilities.Graphviz            (MonadGraphviz (..))

import Control.Monad.Random             (RandT)
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Data.GraphViz                    (quitWithoutGraphviz)
import Data.String.Interpolate          (iii)

instance MonadGraphviz IO where
  errorWithoutGraphviz =
    quitWithoutGraphviz [iii|
      Please install GraphViz executables from http://graphviz.org/
      and put them on your PATH
      |]
  layoutGraph = GV.layoutGraph
  layoutGraph' = GV.layoutGraph'

instance MonadGraphviz (RandT g IO)  where
  errorWithoutGraphviz = lift errorWithoutGraphviz
  layoutGraph command = lift . layoutGraph command
  layoutGraph' params command = lift . layoutGraph' params command
