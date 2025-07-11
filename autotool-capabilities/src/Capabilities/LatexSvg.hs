-- | Defines a Monad context for rendering latex formulas as SVG images.

module Capabilities.LatexSvg (
  MonadLatexSvg (..),
  writeLatexSvg,
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.Maybe                       (fromMaybe)
import Image.LaTeX.Render               (Formula, FormulaOptions(..), SVG, EnvironmentOptions, defaultEnv, defaultFormulaOptions)

class Monad m => MonadLatexSvg m where
  renderImage :: EnvironmentOptions -> FormulaOptions -> Formula -> m SVG
  writeImage :: FilePath -> SVG -> m ()

instance MonadLatexSvg m => MonadLatexSvg (GenericReportT l o m) where
  renderImage opts mEnv = lift . renderImage opts mEnv
  writeImage file = lift . writeImage file


writeLatexSvg
  :: MonadLatexSvg m
  => Maybe EnvironmentOptions
  -> Maybe FormulaOptions
  -> FilePath
  -> Formula
  -> m FilePath
writeLatexSvg env opt path formula = do
  image <- renderImage
    (fromMaybe defaultEnv env)
    (fromMaybe defaultFormulaOptions opt)
    formula
  writeImage path image
  pure path
