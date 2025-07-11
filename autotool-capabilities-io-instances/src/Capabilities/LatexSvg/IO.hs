{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines the IO instance for capability LatexSvg.

module Capabilities.LatexSvg.IO () where

import Capabilities.LatexSvg            (MonadLatexSvg (..))
import Image.LaTeX.Render               (imageForFormula)


instance MonadLatexSvg IO where
  renderImage env opts formula = do
    render <- imageForFormula env opts formula
    case render of
      (Left err)  -> error $ unlines
        ["failed to render an image with the given formula: ", show err]
      (Right svg) -> pure svg

  writeImage = writeFile
