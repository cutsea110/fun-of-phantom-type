{-# LANGUAGE GADTs, ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
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
  RList :: Show a => Type a -> Type [a]
  RPair :: (Show a, Show b) => Type a -> Type b -> Type (a, b)
  RPerson :: Type Person
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
compress (RDyn) a = compressDynamic a

uncompress :: Show t => Type t -> [Bit] -> t
uncompress = ((fst.fromJust).).uncompress'

uncompress' :: Show t => Type t -> [Bit] -> Maybe (t, [Bit])
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
uncompress' (RDyn) bs = uncompressDynamic' bs

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
pretty (RDyn) a = prettyDynamic a

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
parse' (RDyn) cs = parseDynamic $ skipSpace cs

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

data Rep = forall t. Show t => Rep (Type t)

instance Show Rep where
  show (Rep r) = "Rep " ++ show r

compressRep :: Rep -> [Bit]
compressRep (Rep (RInt)) = [O,O,O]
compressRep (Rep (RChar)) = [O,O,I]
compressRep (Rep (RList ra)) = [O,I,O]++compressRep (Rep ra)
compressRep (Rep (RPair ra rb)) = [O,I,I]++compressRep (Rep ra)++compressRep (Rep rb)
compressRep (Rep (RDyn)) = [I,O,O]

uncompressRep :: [Bit] -> Rep
uncompressRep = fst.fromJust.uncompressRep'

uncompressRep' :: [Bit] -> Maybe (Rep, [Bit])
uncompressRep' (O:O:O:bs) = return (Rep RInt, bs)
uncompressRep' (O:O:I:bs) = return (Rep RChar, bs)
uncompressRep' (O:I:O:bs) = 
  uncompressRep' bs >>= \(Rep r, bs') -> return (Rep (RList r), bs')
uncompressRep' (O:I:I:bs) =
  uncompressRep' bs >>= \(Rep r, bs') ->
  uncompressRep' bs' >>= \(Rep r', bs'') ->
  return (Rep (RPair r r'), bs'')
uncompressRep' (I:O:O:bs) = return (Rep RDyn, bs)

compressDynamic :: Dynamic -> [Bit]
compressDynamic (Dyn ra a) = compressRep (Rep ra) ++ compress ra a

uncompressDynamic' :: [Bit] -> Maybe (Dynamic, [Bit])
uncompressDynamic' bs =
  uncompressRep' bs >>= \(Rep ra, bs') ->
  uncompress' ra bs' >>= \(a, bs'') ->
  return (Dyn ra a, bs'')

prettyRep :: Rep -> Doc
prettyRep (Rep (RInt)) = text "RInt"
prettyRep (Rep (RChar)) = text "RChar"
prettyRep (Rep (RList ra)) = lparen <> text "RList" <+> prettyRep (Rep ra) <> rparen
prettyRep (Rep (RPair ra rb)) = align $ cat [lparen, text "RPair", prettyRep (Rep ra), prettyRep (Rep rb), rparen]
prettyRep (Rep (RDyn)) = text "RDyn"

prettyDynamic :: Dynamic -> Doc
prettyDynamic (Dyn ra a) = text "Dyn" <+> prettyRep (Rep ra) <+> (align $ pretty ra a)

parseRep' :: String -> Maybe (Rep, String)
parseRep' cs = parseR $ skipSpace cs
  where
    parseR ('R':'I':'n':'t':cs') = return (Rep RInt, cs')
    parseR ('R':'C':'h':'a':'r':cs') = return (Rep RChar, cs')
    parseR ('R':'L':'i':'s':'t':cs') =
      parseRep' cs' >>= \(Rep ra, cs'') ->
      return (Rep (RList ra), cs'')
    parseR ('R':'P':'a':'i':'r':cs') =
      parseRep' cs' >>= \(Rep ra, cs'') ->
      parseRep' cs'' >>= \(Rep rb, cs''') ->
      return (Rep (RPair ra rb), cs''')
    parseR ('R':'D':'y':'n':cs) = return (Rep RDyn, cs)
    parseR ('(':cs) =
      parseRep' cs >>= \(Rep ra, cs') ->
      parseRP cs' >>= \(_, cs'') ->
      return (Rep ra, cs'')
    parseRP cs = parseRP' $ skipSpace cs
      where
        parseRP' (')':cs) = return (')', cs)

parseDynamic :: String -> Maybe (Dynamic, String)
parseDynamic cs = parseDynamic' $ skipSpace cs
  where
    parseDynamic' ('D':'y':'n':cs) =
      parseRep' cs >>= \(Rep ra, cs') ->
      parse' ra cs' >>= \(a, cs'') ->
      return (Dyn ra a, cs'')

type Name = String
type Age = Int
data Person = Person Name Age deriving Show

tick :: Name -> Traversal
tick s (RPerson) (Person n a) | s == n = Person n (a + 1)
tick s rt t = t

type Traversal = forall t. Type t -> t -> t

copy :: Traversal
copy rt = id

(<*>) :: Traversal -> Traversal -> Traversal
(f <*> g) rt = f rt . g rt

imap :: Traversal -> Traversal
imap f (RInt) i = i
imap f (RChar) c = c
imap f (RList ra) [] = []
imap f (RList ra) (a:as) = f ra a:f (RList ra) as
imap f (RPair ra rb) (a, b) = (f ra a, f rb b)
imap f (RPerson) (Person n a) = Person (f rString n) (f RInt a)

everywhere, everywhere' :: Traversal -> Traversal
everywhere f = f <*> imap (everywhere f)
everywhere' f = imap (everywhere' f) <*> f

type Query s = forall t. Type t -> t -> s

isum :: Query Int -> Query Int
isum f (RInt) i = 0
isum f (RChar) c = 0
isum f (RList ra) [] = 0
isum f (RList ra) (a:as) = f ra a + f (RList ra) as
isum f (RPair ra rb) (a, b) = f ra a + f rb b
isum f (RPerson ) (Person n a) = f rString n + f RInt a

total :: Query Int -> Query Int
total f rt t = f rt t + isum (total f) rt t

age :: Query Age
age (RPerson) (Person n a) = a
age _ _ = 0

sizeof :: Query Int
sizeof (RInt) _ = 2
sizeof (RChar) _ = 2
sizeof (RList ra) [] = 0
sizeof (RList ra) (_:_) = 3
sizeof (RPair ra rb) _ = 3
sizeof (RPerson) _ = 3

-- ps = [Person "Norma" 50, Person "Richard" 59]
-- ps' = everywhere (tick "Richard") (RList RPerson) ps
-- total age ps'
-- total sizeof rString "Richard Bird"

-- imap copy (RInt) i = i
--      copy (RInt) i = id i = i
-- imap copy (RChar) c = c
--      copy (RChar) c = id c = c
-- imap copy (RList ra) [] = []
--      copy (RList ra) [] = id [] = []
-- imap copy (RList ra) (a:as) = copy ra a:copy (RList ra) as = id a:id as = a:as
--      copy (RList ra) (a:as) = id (a:as) = a:as
-- imap copy (RPair ra rb) (a, b) = (copy ra a, copy rb b) = (id a, id b) = (a, b)
--      copy (RPair ra rb) (a, b) = id (a, b) = (a, b)
-- imap copy (RPerson) (Person n a) = Person (copy rString n) (copy RInt a) = Person (id n) (id a) = Person n a
--      copy (RPerson) (Person n a) = id (Person n a) = Person n a
-- ∴ imap copy = copy

