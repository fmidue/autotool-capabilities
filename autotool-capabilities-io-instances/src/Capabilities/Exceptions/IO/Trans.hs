{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Defines transformer instances for exception handling.

module Capabilities.Exceptions.IO.Trans () where

import Control.Monad.Catch              (MonadCatch(..), MonadThrow (..))
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Random       (RandT, liftCatch)

instance MonadThrow (RandT g IO) where
  throwM = lift . throwM

instance MonadCatch (RandT g IO) where
  catch = liftCatch catch
