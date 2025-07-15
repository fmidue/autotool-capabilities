{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability PlantUml.

module Capabilities.PlantUml.IO () where

import qualified Language.PlantUML.Call           as PlantUml (
  DiagramType (SVG),
  drawPlantUMLDiagram,
  )

import Capabilities.PlantUml            (MonadPlantUml (..))

instance MonadPlantUml IO where
  drawPlantUmlSvg = PlantUml.drawPlantUMLDiagram PlantUml.SVG