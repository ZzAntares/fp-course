{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  -- (<$>) f (Compose fg) = Compose $ (\ga -> f <$> ga) <$> fg
  (<$>) f (Compose fg) = Compose $ (f <$>) <$> fg

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure :: a -> Compose f g a
  -- pure x = Compose $ pure $ pure x
  -- pure x = Compose . pure . pure $ x
  pure = Compose . pure . pure

-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  -- (<*>) (Compose fgab) (Compose fga) = Compose $ lift2 (\gab ga -> gab <*> ga) fgab fga
  (<*>) (Compose fgab) (Compose fga) = Compose $ lift2 (<*>) fgab fga
  -- (<*>) (Compose fgab) (Compose fga) = Compose $ (<*>) <$> fgab <*> fga

instance (Monad f, Monad g) => Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  (=<<) = undefined
