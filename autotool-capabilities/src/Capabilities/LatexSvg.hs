-- | Defines a Monad context for rendering latex formulas as SVG images.

module Capabilities.LatexSvg (
  MonadLatexSvg (..),
  writeLatexSvg,
  ) where

import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.ByteString                  (ByteString)
import Data.Maybe                       (fromMaybe)
import Image.LaTeX.Render               (Formula, FormulaOptions(..), EnvironmentOptions, defaultEnv, defaultFormulaOptions)

class Monad m => MonadLatexSvg m where
  renderImage :: EnvironmentOptions -> FormulaOptions -> Formula -> m ByteString

instance MonadLatexSvg m => MonadLatexSvg (GenericReportT l o m) where
  renderImage opts mEnv = lift . renderImage opts mEnv


writeLatexSvg
  :: MonadLatexSvg m
  => Maybe EnvironmentOptions
  -> Maybe FormulaOptions
  -> Formula
  -> m ByteString
writeLatexSvg env = renderImage
  (fromMaybe defaultEnv env)
  . fromMaybe defaultFormulaOptions
