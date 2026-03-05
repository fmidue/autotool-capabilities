{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines an IO based GenericReportT instance for capability PlantUml.

module Capabilities.PlantUml.IO () where

import qualified Language.PlantUML.Call           as PlantUml (
  DiagramType (SVG),
  drawPlantUMLDiagram,
  )

import Capabilities.PlantUml            (MonadPlantUml (..))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )

instance MonadPlantUml (GenericReportT l o IO) where
  drawPlantUmlSvg = lift . PlantUml.drawPlantUMLDiagram PlantUml.SVG
