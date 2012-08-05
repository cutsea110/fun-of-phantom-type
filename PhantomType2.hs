{-# LANGUAGE GADTs, ExistentialQuantification #-}
module PhantomType2 where

s = \x y z -> (x z) (y z)
k = \x y -> x
i = \x -> x

infixr :->

{-
data Type t = RBase
            | forall a b. Type a :-> Type b
-}

data Type t where
  RBase :: Type Base
  (:->) :: forall a b. Type a -> Type b -> Type (a -> b)

data Term t where
  App :: Term (a -> b) -> Term a -> Term b
  Fun :: (Term a -> Term b) -> Term (a -> b)

newtype Base = In { out :: Term Base }

reify :: forall t. Type t -> (t -> Term t)
reify (RBase) v = out v
reify (ra :-> rb) v = Fun (\x -> reify rb (v (reflect ra x)))

reflect :: forall t. Type t -> (Term t -> t)
reflect (RBase) e = In e
reflect (ra :-> rb) e = \x -> reflect rb (App e (reify ra x))


b :: Type Base
b = RBase

