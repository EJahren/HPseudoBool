module HPseudoBool where
import Control.Monad.State
import Control.Monad
import qualified Data.Map as M
import Data.Map(Map)
import Data.List

data Var = X Int deriving (Ord,Eq)

instance Show Var where
 show (X a) = "x" ++ show a

instance Show Line where
  show (Cons c) = show c
  show (Comment str) = "* "++ (intercalate "* \n" . lines $ str)

class Sumable a where
  toSum :: a -> Sum

instance Sumable Var where
  toSum  v = S (M.singleton v 1) 0

instance Sumable Integer where 
  toSum i = S M.empty i

instance Sumable Int where
  toSum i = S M.empty (toInteger i)

instance Sumable Sum where
  toSum = id

data Sum = S {
  vars :: Map Var Integer,
  consts ::  Integer} deriving Eq


emptySum = S (M.empty) 0

data Line = Cons Constraint | Comment String

infixl 3 |*|
a |*| b = S (M.singleton b (toInteger a)) 0

instance Show Sum where 
 show (S m i)  | i < 0 = (unwords . map printSum . M.assocs $ m) ++ " " ++ show i
               | i == 0 = (unwords . map printSum . M.assocs $ m)
               | otherwise = (unwords . map printSum . M.assocs $ m) ++ " +" ++ show i 
  where 
   printSum (a,b) | b == 0 = ""
                  | b > 0  = "+" ++ show b ++ " " ++ show a
                  | otherwise = show b ++ " " ++ show a

(|+|) :: (Sumable a, Sumable b) => a -> b -> Sum
infixl 2 |+|
a |+| b = addS (toSum a) (toSum b)

(|-|) :: (Sumable a, Sumable b) => a -> b -> Sum
infixl 2 |-|
a |-| b = addS (toSum a) (S (M.map negate m) (negate c))
  where (S m c) = toSum b

addS (S xs i) (S ys j) = 
  foldl (\ m (x,y) -> m `g` ( x,y)) (S ys (i+j)) (M.assocs xs)
   where 
     g :: Sum -> (Var,Integer) -> Sum
     g (S m i) (var,c)
       | var `M.member` m = S (M.adjust (+c) var m) i
       | otherwise   = S (M.insert var c m) i

sumC :: (Sumable a) => [a] -> Sum
sumC = foldr (|+|) emptySum

data Constraint = Geq Sum Integer | E Sum Integer 

instance Show Constraint where
  show (Geq (S m i) t) | i /= 0    = error "Can't print Unbalanced Constraint"
                       | otherwise = show (S m i) ++ " >= " ++ show t ++ ";"
  show (E  (S m i)  t) | i /= 0    = error "Can't print Unbalanced Constraint" 
                       | otherwise = show (S m i) ++ " = " ++ show t ++ ";"

(|>=|) :: (Sumable a, Sumable b) => a -> b -> Constraint
infix 1 |>=|
a |>=| b = Geq ((S m 0) |-| (S m' 0)) (c' - c)
  where (S m c) = toSum a
        (S m' c') = toSum b
(|==|) :: (Sumable a, Sumable b) => a -> b -> Constraint
infix 1 |==|
a |==| b = E ((S m 0) |-| S m' 0) (c' - c)
  where (S m c) = toSum a
        (S m' c') = toSum b
(|<=|) :: (Sumable a, Sumable b) => a -> b -> Constraint
infix 1 |<=|
a |<=| b = ((toSum b) |-| (toSum a)) |>=| asSum 0
(|>|) :: (Sumable a, Sumable b) => a -> b -> Constraint
infix 1 |>|
a |>| b = Geq (S m 0 |-| S m' 0) (c'- c + 1)
  where (S m c) = toSum a
        (S m' c') = toSum b
(|<|) :: (Sumable a, Sumable b) => a -> b -> Constraint
infix 1 |<|
a |<| b = Geq ((S m 0) |-| (S m' 0)) (c' - c -1)
  where (S m c) = toSum a
        (S m' c') = toSum b

data PBencState = PBS {
  nexti :: Int,
  constraints :: [Line],
  objective :: Sum
}

type PBencM = State PBencState

fresh :: PBencM Var
fresh = do 
  i <- gets nexti
  modify (\p -> p{nexti = ((nexti p)+1)})
  return (X i)

freshs :: Int -> PBencM [Var]
freshs i = replicateM i fresh

add :: Constraint -> PBencM ()
add c = modify (\s -> s{constraints = (Cons c):(constraints s)})

comment :: String -> PBencM ()
comment str = modify (\s -> s{constraints = (Comment str):(constraints s)})

addAll xs = mapM_ add xs

minimize :: Sum -> PBencM ()
minimize c = modify (\s -> s{objective = c})

getEncoding :: PBencM a -> String
getEncoding p = pr (execState p (PBS 1 [] emptySum))
  where
   pr (PBS i cs' s) =  "* #variable= " ++ show (i-1) ++
                         " #constraint= " ++ 
                          show (sum . map f $ cs)
                        ++ "\n" ++
                      "min: " ++ show s ++ ";\n" ++
                      unlines (map show cs)
     where cs = reverse cs'
           f (Cons _ ) = 1
           f _          = 0
--instance Num Sum where
--   (+) = (|+|)
--   (-) = (|-|)
--   (*) = undefined
--   signum = undefined
--   abs = undefined
--   negate (S m i) = S (M.map negate m) (negate i)
--   fromInteger i = (S M.empty i)

asSum :: (Integral a) => a -> Sum
asSum = toSum . toInteger

--instance (Integral a) => Sumable a where
--  toSum = asSum

test = do
  add (2 |*| X 1 |+| 3 |*| X 2 |<=| (asSum 3))
  add (3 |*| X 2 |+| 5 |*| X 3 |>| (asSum 4))
  minimize (X 1 |+| X 2)

test2 = do
  add (X 2 |<=| X 3)
  add (3 |*| X 2 |+| 5 |*| X 3 |+| (asSum 4) |>=| (asSum 4) |+| X 3)
  minimize (X 1 |+| X 2 |-| (asSum 1))

test3 =
  add (X 1  |<=|  ((2 * 0)-1) |*| X 2 |+| (asSum 1) |-| (asSum 0))
