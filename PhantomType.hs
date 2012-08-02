{-# LANGUAGE GADTs, ExistentialQuantification #-}
module PhantomType where

import Data.Char

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
{--
data Type t = RInt
            | RChar
            | forall a. RList (Type a)
            | forall a b. RPair (Type a) (Type b)
--}
data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)
  
rString :: Type String
rString = RList RChar

data Bit = O | I
instance Show Bit where
  show = show . toChar
  showList = showList . map toChar

toChar :: Bit -> Char
toChar O = '0'
toChar I = '1'

intToBits :: Int -> [Bit]
intToBits n = take 32 $ intToBits' n ++ repeat O
  where
    intToBits' :: Int -> [Bit]
    intToBits' n | n == 0 = [O]
                 | n == 1 = [I]
                 | otherwise = i2b m:intToBits d
    (d, m) = (n `div` 2, n `mod` 2)
    i2b :: Int -> Bit
    i2b 0 = O
    i2b 1 = I

charToBits :: Char -> [Bit]
charToBits c = take 7 $ (intToBits $ ord c) ++ repeat O

compress :: forall t. Type t -> t -> [Bit]
compress (RInt) i = compressInt i
  where compressInt = intToBits
compress (RChar) c = compressChar c
  where compressChar = charToBits
compress (RList ra) [] = O:[]
compress (RList ra) (a:as) = I:compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a,b) = compress ra a ++ compress rb b
