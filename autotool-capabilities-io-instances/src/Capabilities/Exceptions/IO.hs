{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Defines exception handling instances for IO based randomization.

module Capabilities.Exceptions.IO () where

import Control.Monad.Catch              (MonadCatch(..), MonadThrow (..))
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Random       (RandT, liftCatch)

instance MonadThrow (RandT g IO) where
  throwM = lift . throwM

instance MonadCatch (RandT g IO) where
  catch = liftCatch catch
