-- | Defines a Monad context for rendering diagrams graphics to file.

module Capabilities.Diagrams (
  MonadDiagrams (lin, renderDiagram),
  ) where

import Control.Monad.Random                       (RandT)
import Control.Monad.Trans.Class                  (MonadTrans (lift))
import Control.OutputCapable.Blocks.Generic (
  GenericReportT
  )
import Data.ByteString                            (ByteString)
import Data.Data                                  (Typeable)
import Diagrams.Backend.SVG                       (SVG)
import Diagrams.Prelude                           (QDiagram)
import Diagrams.TwoD                              (V2)
import Graphics.SVGFonts.ReadFont                 (PreparedFont)

class Monad m => MonadDiagrams m where
  lin :: (Read n, RealFloat n) => m (PreparedFont n)
  renderDiagram
    :: (Show n, Typeable n, RealFloat n, Monoid o)
    => QDiagram SVG V2 n o
    -> m ByteString

instance MonadDiagrams m => MonadDiagrams (GenericReportT l o m)  where
  lin = lift lin
  renderDiagram = lift . renderDiagram

instance MonadDiagrams m => MonadDiagrams (RandT g m) where
  lin = lift lin
  renderDiagram = lift . renderDiagram
