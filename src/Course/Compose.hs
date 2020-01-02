{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose a) = Compose $ ((f <$>) <$>) a
    -- error "todo: Course.Compose (<$>)#instance (Compose f g)"



-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- Functor Laws:
-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)





-- Applicative Laws:
-- pure id <*> v = v
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w


instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure x =
    -- error "todo: Course.Compose pure#instance (Compose f g)"
    Compose $ pure $ pure x
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose x) (Compose y) =
    -- error "todo: Course.Compose (<*>)#instance (Compose f g)"
    Compose $ (lift2 . lift2) ($) x y
  -- (<*>) = _todo

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
