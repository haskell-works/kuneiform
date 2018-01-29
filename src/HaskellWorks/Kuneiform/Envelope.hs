module HaskellWorks.Kuneiform.Envelope where

import Data.Bifunctor

data Envelope c p = Envelope
  { envelopeContext :: c
  , envelopePayload :: p
  } deriving (Eq, Show)

instance Functor (Envelope c) where
  fmap f (Envelope c a) = Envelope c (f a)

instance Bifunctor Envelope where
  bimap f g (Envelope c a) = Envelope (f c) (g a)

instance Traversable (Envelope c) where
  traverse f (Envelope c a) = Envelope c <$> f a

instance Foldable (Envelope c) where
  foldMap f (Envelope _ y) = f y

  foldr f z (Envelope _ y) = f y z
