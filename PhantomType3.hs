{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module PhantomType3 where

newtype a :=: b = Proof { apply :: forall p. p a -> p b }

refl :: forall a. a :=: a
refl = Proof id
