{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines an IO based GenericReportT instance for capability LatexSvg.

module Capabilities.LatexSvg.IO () where

import Capabilities.LatexSvg            (MonadLatexSvg (..))
import Control.Monad.Trans.Class        (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.Text                        (pack)
import Data.Text.Encoding               (encodeUtf8)
import Image.LaTeX.Render               (imageForFormula)

instance MonadLatexSvg (GenericReportT l o IO) where
  renderImage env opts formula = lift $ do
    render <- imageForFormula env opts formula
    case render of
      (Left err)  -> error $ unlines
        ["failed to render an image with the given formula: ", show err]
      (Right svg) -> pure $ encodeUtf8 $ pack svg
