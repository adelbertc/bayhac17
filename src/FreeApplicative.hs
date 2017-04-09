{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module FreeApplicative(FreeApplicative, interpret, lift) where

import Prelude

data FreeApplicative f a = Pure a
                         | forall p. Ap (FreeApplicative f (p -> a)) (f p)

interpret :: Applicative g => FreeApplicative f a -> (forall x. f x -> g x) -> g a
interpret (Pure a) _ = pure a
interpret (Ap fn p) nt = interpret fn nt <*> nt p

lift :: f a -> FreeApplicative f a
lift = Ap (Pure id)





instance Functor (FreeApplicative a) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap fg p) = Ap (fmap (f .) fg) p

instance Applicative (FreeApplicative f) where
  (Pure f) <*> fa = fmap f fa
  (Ap fn p) <*> fa = Ap (fmap flip fn <*> fa) p
  pure = Pure
