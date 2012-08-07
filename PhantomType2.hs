{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
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

data Btree a = Leaf a
             | Fork (Btree a) (Btree a)
             deriving (Show)

flatten :: forall a. Btree a -> [a]
flatten t = flatcat t []

flatcat :: forall a. Btree a -> [a] -> [a]
flatcat (Leaf a) as = a:as
flatcat (Fork tl tr) as = flatcat tl (flatcat tr as)

data Dir t p where
  Lit :: String -> Dir t t
  Int :: Dir t (Int -> t)
  String :: Dir t (String -> t)
  (:^:) :: Dir p1 p2 -> Dir t p1 -> Dir t p2

format' :: forall t p. Dir t p -> (String -> t) -> (String -> p)
format' (Lit s) = \cont out -> cont (out ++ s)
format' (Int) = \cont out -> \i -> cont (out ++ show i)
format' (String) = \cont out -> \s -> cont (out ++ s)
format' (d1 :^: d2) = \cont out -> format' d1 (format' d2 cont) out

format :: forall p. Dir String p -> p
format d = format' d id ""
