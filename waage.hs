import Prelude hiding (Left, Right)
import Test.QuickCheck hiding (Result)
import Data.List

data Position = Position { unknown :: Int -- balls wich nothing known about them
                         , heavy   :: Int -- balls with could be heavy but not light
                         , light   :: Int -- balls with could be light but not heavy
                         , normal  :: Int -- balls with normal weight
                         } deriving (Show, Eq)
data Label = Unknown | Heavy | Light | Normal
           deriving (Show, Eq)


instance Arbitrary Position where
  arbitrary = sized $ \n -> do
    let n' = n + 3
    labels <- sequence $ replicate n' $ oneof $ map return all_labels 
    return $ foldl (\p l -> set_label l (length $ filter (== l) labels) p) emptyPosition all_labels

data QCPosMove = QCPosMove Position Move
                 deriving (Show)

instance Arbitrary QCPosMove where
  arbitrary = sized $ \n -> do
    let n'  = n + 3
        n'' = 1 `max` (n `div` 3)
    pos <- resize (n' - n'') arbitrary
    mov <- oneof $ map return $ take n'' $ possible_moves pos
    return $ QCPosMove pos mov

type Scale = Position

data Move = Move Scale Scale
            deriving (Show)

instance Eq Move where
  (==) (Move l1 r1) (Move l2 r2) = (l1 == l2 && r1 == r2) || (l1 == r2 && r1 == l2)

data Result = Equal | Left | Right
            deriving (Show, Eq)

instance Arbitrary Result where
  arbitrary = oneof [return Equal, return Left, return Right]

all_labels :: [Label]
all_labels = [Unknown, Heavy, Light, Normal]

set_label :: Label -> Int -> Position -> Position
set_label Unknown i p = p {unknown = i}
set_label Heavy   i p = p {heavy   = i}
set_label Light   i p = p {light   = i}
set_label Normal  i p = p {normal  = i}

get_label :: Label -> Position -> Int
get_label Unknown = unknown
get_label Heavy   = heavy
get_label Light   = light
get_label Normal  = normal

-- add_label :: Label -> Int -> Position -> Position
-- add_label l i p = set_label l (get_label l p ++ i) p

total :: Position -> Int
total p = sum $ [get_label f p | f <- all_labels]

emptyPosition :: Position
emptyPosition = Position {unknown = 0, heavy = 0, light = 0, normal = 0}

startPosition :: Int -> Position
startPosition i = set_label Unknown i emptyPosition

sub :: Position -> Position -> Position
sub a b = foldl sub_label  a all_labels
  where
    sub_label p l = set_label l (get_label l a - get_label l b) p

prop_sub p1 p2 = total (p1 `sub` p2) == (total p1 - total p2)

possible_scales :: Position -> Int -> [Scale]
possible_scales p n = possible_scales' p all_labels n
  where
    possible_scales' _ _ 0      = [emptyPosition]
    possible_scales' p (l:ls) n = let m     = min n (get_label l p)
                                      sub i = map (set_label l i) $ possible_scales' p ls (n - i)
                                  in concat [sub i | i <- [0..m]]
    possible_scales' _ [] _     = []

possible_moves :: Position -> [Move]
possible_moves p = nub $ [Move a b | k <- [1..n], a <- possible_scales p k, b <- other_scales a]
  where
    n = total p `div` 2
    other_scales s = possible_scales (p `sub` s) (total s)

apply_move :: Position -> Move -> [Position]
apply_move pos move = nub [apply_result res pos move | res <- [Equal, Left, Right]]

apply_result :: Result -> Position -> Move -> Position
apply_result Equal p (Move l r) = let p' = p `sub` l `sub` r 
                                  in p' {normal = normal p' + total l + total r}
apply_result Left  p (Move le ri) = let he = unknown le + heavy le
                                        li = unknown ri + light ri
                                  in Position { unknown = 0
                                              , heavy   = he
                                              , light   = li
                                              , normal  = total p - he - li
                                              }
apply_result Right p (Move l r) = apply_result Left p (Move r l)

prop_apply_result (QCPosMove p m) res = total p == total (apply_result res p m)

bad_move :: Position -> Move -> Bool
bad_move pos = any nowin . apply_move pos
  where
    nowin new = (unknown new >= unknown pos) && (normal new <= normal pos)

good_moves :: Position -> [Move]
good_moves pos = filter (not . bad_move pos) $ possible_moves pos

prop_good_moves p = (total p > 2) ==> (normal p >= total p - 1) == (null $ good_moves p)

depth :: Position ->  Int
depth pos
  | normal pos >= total pos  - 1 = 0
  | otherwise                    = 1 + (minimum'
                                        $ map (maximum . map depth . apply_move pos) 
                                        $ good_moves pos)
  where
    minimum' [] = 1000
    minimum' l  = minimum l

prop_depth p = (total p > 2 && total p < 6) ==> depth p < 1000
                    

