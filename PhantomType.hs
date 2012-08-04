{-# LANGUAGE GADTs, ExistentialQuantification #-}
module PhantomType where

import Control.Arrow (first, (***))
import Control.Monad (liftM, liftM2)
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Data.List (unfoldr)
import Text.PrettyPrint.Leijen hiding (pretty, list)

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
  RDyn :: Type Dynamic

data Dynamic = forall t. Show t => Dyn (Type t) t

instance Show (Type t) where
  show (RInt) = "RInt"
  show (RChar) = "RChar"
  show (RList ra) = "(RList " ++ show ra ++ ")"
  show (RPair ra rb) = "(RPair " ++ show ra ++ " " ++ show rb ++ ")"
  show (RDyn) = "RDyn"

instance Show Dynamic where
  show (Dyn ra a) = "Dyn " ++ show ra ++ " " ++ show a

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

bitsToInt' :: Int -> [Bit] -> Maybe (Int, [Bit])
bitsToInt' n bs = if length h == n
                  then Just (sum $ zipWith (\b x -> b2i b*2^x) h [0..], t)
                  else Nothing
  where
    (h, t) = splitAt n bs
    b2i O = 0
    b2i I = 1

bitsToInt :: [Bit] -> Maybe (Int, [Bit])
bitsToInt = bitsToInt' 32

charToBits :: Char -> [Bit]
charToBits c = take 7 $ (intToBits $ ord c) ++ repeat O

bitsToChar :: [Bit] -> Maybe (Char, [Bit])
bitsToChar = fmap (first chr) . bitsToInt' 7

compress :: forall t. Type t -> t -> [Bit]
compress (RInt) i = compressInt i
  where compressInt = intToBits
compress (RChar) c = compressChar c
  where compressChar = charToBits
compress (RList ra) [] = O:[]
compress (RList ra) (a:as) = I:compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a,b) = compress ra a ++ compress rb b

uncompress :: forall t. Type t -> [Bit] -> t
uncompress = ((fst.fromJust).).uncompress'

uncompress' :: forall t. Type t -> [Bit] -> Maybe (t, [Bit])
uncompress' (RInt) bs = uncompressInt bs
  where uncompressInt = bitsToInt
uncompress' (RChar) bs = uncompressChar bs
  where uncompressChar = bitsToChar
uncompress' (RList ra) (O:bs) = Just ([], bs)
uncompress' (RList ra) (I:bs) =
  uncompress' ra bs >>= \(a, bs') -> 
  uncompress' (RList ra) bs' >>= \(as, bs'') ->
  return (a:as, bs'')
uncompress' (RPair ra rb) bs = 
  uncompress' ra bs >>= \(a, bs') ->
  uncompress' rb bs' >>= \(b, bs'') ->
  return ((a, b), bs'')

pretty :: forall t. Type t -> t -> Doc
pretty (RInt) i = prettyInt i
  where prettyInt = text . show
pretty (RChar) c = prettyChar c
  where prettyChar = text . show
pretty (RList RChar) s = prettyString s
  where prettyString = text . show
pretty (RList ra) [] = text "[]"
pretty (RList ra) (a:as) = block 1 (text "[" <> pretty ra a <> prettyL as)
  where
    prettyL [] = text "]"
    prettyL (a:as) = text "," <> line <> pretty ra a <> prettyL as
pretty (RPair ra rb) (a, b) = block 1 (text "(" <> pretty ra a <> text ","
                                       <> line <> pretty rb b <> text ")")

block :: Int -> Doc -> Doc
block i d = group (nest i d)

parse :: forall t. Type t -> String -> t
parse = ((fst.fromJust).).parse'

parse' :: forall t. Type t -> String -> Maybe (t, String)
parse' (RInt) cs = parseInt $ skipSpace cs
  where 
    parseInt cs = let (is, cs') = span isDigit cs  
                  in return (read is :: Int, cs')
parse' (RChar) cs = parseChar $ skipSpace cs
  where parseChar ('\'':c:'\'':cs') = Just (c, cs')
parse' (RList RChar) cs = parseList $ skipSpace cs
  where
    parseList ('"':cs) = let (f, '"':s) = span (/='"') cs
                         in return (f, s)
parse' (RList ra) cs = parseList ra $ skipSpace cs
  where
    parseList :: forall t. Type t -> String -> Maybe ([t], String)
    parseList ra ('[':cs) = parseList' ra cs
    parseList' :: forall t. Type t -> String -> Maybe ([t], String)
    parseList' ra (']':cs) = return ([], cs)
    parseList' ra cs = 
      parse' ra cs >>= \(a, cs') ->
      parseSep cs' >>= \(sep, cs'') ->
      case sep of
        ']' -> return ([a], cs'')
        ',' -> parseList' ra cs'' >>= \(as, cs''') ->
          return (a:as, cs''')
        _ -> fail "illegal list"
parse' (RPair ra rb) cs = parsePair ra rb $ skipSpace cs
  where
    parsePair ra rb ('(':cs) =
      parse' ra cs >>= \(a, cs') ->
      parseSep cs' >>= \(',', cs'') ->
      parse' rb cs'' >>= \(b, cs''') ->
      parseSep cs''' >>= \(')', cs'''') ->
      return ((a, b), cs'''')

skipSpace :: String -> String
skipSpace ccs@(c:cs) | isSpace c = skipSpace cs
                     | otherwise = ccs

parseSep :: String -> Maybe (Char, String)
parseSep ccs@(c:cs) | c `elem` ",])" = Just (c, cs)
                    | otherwise = Nothing

eq :: forall t. Type t -> t -> t -> Bool
eq (RInt) i i' = i == i'
eq (RChar) c c' = c == c'
eq (RList _) [] [] = True
eq (RList _) (_:_) [] = False
eq (RList _) [] (_:_) = False
eq (RList ra) (a:as) (b:bs) = eq ra a b && eq (RList ra) as bs
eq (RPair ra rb) (a, b) (a', b') = eq ra a a' && eq rb b b'

-- testI = eq RInt 34 35
-- testC = eq RChar 'c' 'C'
-- testL = eq (RList RChar) "abvc" "abvc"
-- testP = eq (RPair RChar rString) ('a', "abc") ('b', "xyz")

comp :: forall t. Type t -> t -> t -> Ordering
comp (RInt) i i' = i `compare` i'
comp (RChar) c c' = c `compare` c'
comp (RList _) [] [] = EQ
comp (RList _) (_:_) [] = GT
comp (RList _) [] (_:_) = LT
comp (RList ra) (a:as) (b:bs) = comp ra a b `mappend` comp (RList ra) as bs
comp (RPair ra rb) (a, b) (a', b') = comp ra a a' `mappend` comp rb b b'

-- testI = comp RInt 3 4
-- testC = comp RChar 'a' 'b'
-- testL = comp (RList RInt) [1,2,3,4] [1,2,4,2]
-- testP = comp (RPair RInt rString) (60, "Richard") (59, "Richard")

tequal :: forall t s. Type t -> Type s -> Maybe (t -> s)
tequal (RInt) (RInt) = return id
tequal (RChar) (RChar) = return id
tequal (RList ra1) (RList ra2) = liftM list (tequal ra1 ra2)
tequal (RPair ra1 rb1) (RPair ra2 rb2) = liftM2 pair (tequal ra1 ra2) (tequal rb1 rb2)
tequal _ _ = fail "cannot unify"

list :: (a -> b) -> ([a] -> [b])
list = map
pair :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
-- arr a c -> arr b d -> arr (a, b) (c, d)
pair = (***)

cast :: forall t. Dynamic -> Type t -> Maybe t
cast (Dyn ra a) rt = fmap (\f -> f a) (tequal ra rt)
