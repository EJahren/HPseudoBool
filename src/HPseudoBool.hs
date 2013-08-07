{-# LANGUAGE FlexibleInstances, UndecidableInstances,BangPatterns#-}
module HPseudoBool
  (Var(..),
   Sum(..),
   Line(..),
   emptySum,
   (|==|),
   (|>=|),
   (|<=|),
   (|*|),
   (|<|),
   (|>|),
   getEncoding,
   newVar,
   newVar1,
   sumC,
   Constraint(..),
   PBencM,
   PBencState(..),
   add,
   comment,
   addAll,
   minimize
   ) where
import Control.Monad.State.Strict
import Control.Monad
import qualified Data.Map as M
import Data.Map(Map)
import Data.List
import qualified Data.Set as S

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Builder

import Data.Monoid hiding (Sum)

data Var = X Int deriving (Ord,Eq)

instance Show Var where
 show (X a) = "x" ++ show a

instance Show Line where
  show (Cons c) = show c
  show (Comment str) = "* "++ (intercalate "* \n" . lines $ str)

data Sum = S {
  vars :: Map Var Int,
  consts ::  Int} deriving Eq


emptySum = S (M.empty) 0

data Line = Cons Constraint | Comment String

infixl 3 |*|
a |*| v = S (M.singleton v a) 0

instance Show Sum where 
 show (S m i)  | i < 0 = (unwords . map printSum . M.assocs $ m) ++ " " ++ show i
               | i == 0 = (unwords . map printSum . M.assocs $ m)
               | otherwise = (unwords . map printSum . M.assocs $ m) ++ " +" ++ show i 
  where 
   printSum (a,b) | b == 0 = ""
                  | b > 0  = "+" ++ show b ++ " " ++ show a
                  | otherwise = show b ++ " " ++ show a

instance Num Sum where
  a - b = a + (negate b)
  (+) = addS
  negate (S xs i) = (S (M.map negate xs) (negate i))
  abs = undefined
  signum = undefined
  (*) = undefined
  fromInteger i = (S M.empty (fromInteger i))

addS (S xs i) (S ys j) = 
  foldl (\ m (x,y) -> m `g` ( x,y)) (S ys (i+j)) (M.assocs xs)
   where 
     g :: Sum -> (Var,Int) -> Sum
     g (S m i) (var,c)
       | var `M.member` m = S (M.adjust (+c) var m) i
       | otherwise   = S (M.insert var c m) i

sumC :: [Sum] -> Sum
sumC = foldr (+) emptySum

data Constraint = Geq Sum Int | E Sum Int 

getSum :: Constraint -> Sum
getSum (Geq s _) = s
getSum (E s _) = s

instance Show Constraint where
  show (Geq (S m i) t) | i /= 0    = error "Can't print Unbalanced Constraint"
                       | otherwise = show (S m i) ++ " >= " ++ show t ++ ";"
  show (E  (S m i)  t) | i /= 0    = error "Can't print Unbalanced Constraint" 
                       | otherwise = show (S m i) ++ " = " ++ show t ++ ";"



(|>=|) :: Sum -> Sum -> Constraint
infix 1 |>=|
(S m c) |>=| (S m' c') = Geq ((S m 0) - (S m' 0)) (c' - c)
(|==|) :: Sum -> Sum -> Constraint
infix 1 |==|
(S m c) |==| (S m' c') = E ((S m 0) - S m' 0) (c' - c)
(|<=|) :: Sum -> Sum -> Constraint
infix 1 |<=|
a |<=| b = (b - a) |>=| 0
(|>|) :: Sum -> Sum -> Constraint
infix 1 |>|
(S m c) |>| (S m' c') = Geq (S m 0 - S m' 0) (c'- c + 1)
(|<|) ::  Sum -> Sum -> Constraint
infix 1 |<|
(S m c) |<| (S m' c') = Geq ((S m 0) - (S m' 0)) (c' - c -1)

data PBencState = PBS {
  nextFresh :: {-# UNPACK #-} !Int,
  assocs :: Map Var String,
  constraints :: [Line],
  objective :: Sum
}

type PBencM = State PBencState

newVar :: String -> PBencM Sum
newVar str = do
  i <- gets nextFresh
  let retVar = X i
  modify 
    (\p -> 
       p{nextFresh = i + 1,
         assocs = M.insert retVar str (assocs p) })
  return (S (M.singleton retVar 1) 0)

newVar1 :: PBencM Sum
newVar1 = do
  i <- gets nextFresh
  modify (\p -> p{nextFresh = i + 1})
  return (S (M.singleton (X i) 1) 0)

add :: Constraint -> PBencM ()
add c = modify (\s -> s{constraints = (Cons c) : constraints s})

comment :: String -> PBencM ()
comment str = modify (\s -> s{constraints = (Comment str):(constraints s)})

addAll xs = mapM_ add xs

minimize :: Sum -> PBencM ()
minimize c = modify (\s -> s{objective = c})

getEncoding :: PBencM a -> L.ByteString
getEncoding p = toLazyByteString (pr (execState p (PBS 1 M.empty [] emptySum)))
  where
    pr (PBS i m cs' s) =  varBldr <> conBldr <> charUtf8 '\n' <> minBldr <> constrs
     where 
       varBldr = stringUtf8 "* #variable= " <> intDec (i-1)
       conBldr = stringUtf8 " #constraint= " <> intDec (sum . map f $ cs)
       minBldr = if s /= S M.empty 0 then 
         stringUtf8 "min: " <> 
          sumToBldr s <> stringUtf8 ";\n" else mempty
       constrs = 
         mconcat (map lineToBldr cs)
       cs = reverse cs'
       f (Cons _ ) = 1
       f _          = 0

sumToBldr (S m i)
    | i < 0 = mconcat
      [mconcat . map printSum . M.assocs $ m,
       intDec i]
    | i == 0  = mconcat . map printSum . M.assocs $ m
    | otherwise = mconcat
      [mconcat . map printSum . M.assocs $ m,
       charUtf8 '+',
       intDec i]
    where
      printSum (X a,b)
        | b == 0 = mempty
        | b > 0 = charUtf8 '+' <> intDec b <> stringUtf8 " x" <> intDec a <> charUtf8 ' '
        | otherwise =  intDec b <> stringUtf8 " x" <>  intDec a


lineToBldr (Comment str) = stringUtf8 "* " <> stringUtf8 str <> charUtf8 '\n'
lineToBldr (Cons c) = constrToBldr c

constrToBldr (Geq (S m i) t) =
  sumToBldr (S m i) <> stringUtf8 " >= " <> intDec t <> stringUtf8 ";\n"
constrToBldr (E (S m i) t) =
  sumToBldr (S m i) <> stringUtf8 " = " <> intDec t <> stringUtf8 ";\n"



test = do
  c <- newVar1
  add ( c |<=| 1)
  [a,b] <- mapM newVar ["a","b"]
  add (c |>=| a + b)
  minimize( a - b)
