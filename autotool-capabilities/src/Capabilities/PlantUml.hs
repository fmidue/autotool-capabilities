-- | Defines a Monad context for rendering PlantUML graphics to file.

module Capabilities.PlantUml (
  MonadPlantUml (drawPlantUmlSvg),
  ) where

import Data.ByteString                  (ByteString)

class Monad m => MonadPlantUml m where
  drawPlantUmlSvg :: ByteString -> m ByteString
