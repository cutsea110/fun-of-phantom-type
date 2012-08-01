{-# LANGUAGE GADTs, ExistentialQuantification #-}
module PhantomType where

{--
data Term t = Zero
            | Succ (Term Int)
            | Pred (Term Int)
            | IsZero (Term Int)
            | forall a. If (Term Bool) (Term a) (Term a)
--}

data Term t where
  Zero :: Term Int
  Succ :: Term Int -> Term Int
  Pred :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a

eval :: forall a. Term a -> a
eval Zero = 0
eval (Succ e) = eval e + 1
eval (Pred e) = eval e - 1
eval (IsZero e) = eval e == 0
eval (If b t e) = if eval b then eval t else eval e

instance Show a => Show (Term a) where
  show = show . eval

zero, one, two :: Term Int
(zero, one, two) = (Zero, Succ zero, Succ one)

true, false :: Term Bool
(true, false) = (IsZero zero, IsZero one)

plus :: Term Int -> Term Int -> Term Int
plus Zero     y = y
plus (Succ x) y = plus x (Succ y)
plus (Pred x) y = plus x (Pred y)
