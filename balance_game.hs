{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
import Prelude hiding (Left, Right)
import Data.Generics hiding (GT)
import Data.List
import qualified Data.Map as M
import Maybe
import Monad
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ

import qualified System.Console.CmdArgs as CMD
import System.Console.CmdArgs ((&=))
import System.Environment

import Test.Framework.TH
import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck hiding (Result)
import Test.HUnit

import Criterion.Main
import Criterion.Config hiding (Normal)

data Args = Calc { positions :: [String]
                 , perfect   :: Bool
                 , solution  :: Bool
                 }
          | Test
          | Benchmark
          deriving (Show, Data, Typeable)

args = CMD.modes
       [ Calc { positions = []    &= CMD.args
              , perfect   = False &= CMD.name "p"
                                  &= CMD.help ("Only consider solutions where it is known"
                                               ++ " if the ball is too heavy or too light.")
              , solution  = False &= CMD.name "s"
                                  &= CMD.help ("Show the best solution")
              } &= CMD.auto
                &= CMD.details [ "In the balance game, you need to find the *one*"
                                 ++ " ball with the wrong weight among a given number"
                                 ++ " of balls using a balance scale. This program finds"
                                 ++ " optimal solutions to this game."
                               ]
       , Test &= CMD.details ["Run some tests for this program."]
       , Benchmark &= CMD.details ["Run some benchmarks for this program."]
       ]
       &= CMD.program "balance_scale"

main :: IO ()
main = do
  args <- CMD.cmdArgs args
  case args of
    Calc {positions = [], perfect = p, solution = s} -> handle_pos p s $ start_position 13
    Calc {positions = ps, perfect = p, solution = s} -> sequence_ $ map (handle_pos p s . read) ps
    Test                                             -> withArgs [] $ $(defaultMainGenerator)
    Benchmark                                        -> withArgs [] $ defaultMain benchmarks
  where
    cond True  = is_perfect_solution
    cond False = is_solution
    result True  p pos = "\n" ++ (show $ solution_with (cond p) pos)
    result False p pos = show $ depth_with (cond p) pos
    handle_pos p s pos = putStrLn $ show pos ++ ": " ++ result s p pos ++ "\n"
    benchmarks         = concat $ map benchmark_pos ["U3", "U5", "U13"]
    benchmark_pos s    = [ bench ("unknown depth "    ++ s) $ whnf depth_unknown    (read s)
                         , bench ("perfect depth "    ++ s) $ whnf depth_perfect    (read s)
                         , bench ("unknown solution " ++ s) $ whnf solution_unknown (read s)
                         ]

data Position = Position { unknown :: Int -- balls with completely unknown weight
                         , heavy   :: Int -- balls which could be heavy but not light
                         , light   :: Int -- balls which could be light but not heavy
                         , normal  :: Int -- balls with normal weight
                         } deriving (Eq, Ord, Data, Typeable)
data Label = Unknown | Heavy | Light | Normal
           deriving (Eq)

instance Show Position where
  show p = let f c l = case get_label l p of
                            0 -> ""
                            n | n < 4     -> take n $ repeat c
                              | otherwise -> '(' : c : show n ++ ")"
           in concat $ zipWith f  "UHLN" [Unknown, Heavy, Light, Normal]

instance Read Position where
  readsPrec _ = either (const []) id . parse parsecRead ""
    where
      label = do
        c <- option 'U' $ oneOf "UHLN"
        n <- many1 digit
        let l = fromJust $ lookup c $ zip "UHLN" [Unknown, Heavy, Light, Normal]
        return (l, read n)
      set_label' p (l, n) = set_label l n p
      parsecRead = do
        xs <- many1 label
        rest <- getInput
        let pos = foldl set_label' empty_position xs
        return [(pos, rest)]

instance Arbitrary Position where
  arbitrary = sized $ \n -> do
    let n' = (n + 3) `min` 10
    labels <- sequence $ replicate n' $ oneof $ map return all_labels
    return $ foldl (\p l -> set_label l (length $ filter (== l) labels) p) empty_position all_labels

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
  arbitrary = oneof $ map return [Equal, Left, Right]

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
total p = sum [get_label f p | f <- all_labels]

empty_position :: Position
empty_position = Position {unknown = 0, heavy = 0, light = 0, normal = 0}

start_position :: Int -> Position
start_position i = set_label Unknown i empty_position

sub :: Position -> Position -> Position
sub a b = foldl sub_label  a all_labels
  where
    sub_label p l = set_label l (get_label l a - get_label l b) p

prop_sub :: Position -> Position -> Bool
prop_sub p1 p2 = total (p1 `sub` p2) == (total p1 - total p2)

possible_scales :: Position -> Int -> [Scale]
possible_scales p n = possible_scales' p all_labels n
  where
    possible_scales' _ _ 0      = [empty_position]
    possible_scales' p (l:ls) n = let m     = min n (get_label l p)
                                      sub i = map (set_label l i) $ possible_scales' p ls (n - i)
                                  in concat [sub i | i <- [0..m]]
    possible_scales' _ [] _     = []

possible_moves :: Position -> [Move]
possible_moves p = nub $ [Move a b | k <- [1..n], a <- possible_scales p k, b <- other_scales a]
  where
    n = total p `div` 2
    other_scales s = possible_scales (p `sub` s) (total s)

prop_possible_moves_less_balls :: Position -> Bool
prop_possible_moves_less_balls p = all less_balls $ possible_moves p
  where
    less_balls (Move l r) = total l + total r <= total p

prop_possible_moves_same_num :: Position -> Bool
prop_possible_moves_same_num p = all same_num $ possible_moves p
  where
    same_num (Move l r) = total l == total r

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

prop_apply_result_same_total :: QCPosMove -> Result -> Bool
prop_apply_result_same_total (QCPosMove p m) res = total p == total (apply_result res p m)

prop_apply_result_positive :: QCPosMove -> Result -> Bool
prop_apply_result_positive (QCPosMove p m) res = let p' = apply_result res p m
                                                 in all (\l -> get_label l p' >= 0) all_labels

bad_move :: Position -> Move -> Bool
bad_move pos = any nowin . apply_move pos
  where
    nowin new = (unknown new >= unknown pos) && (normal new <= normal pos)

good_moves :: Position -> [Move]
good_moves pos = filter (not . bad_move pos) $ possible_moves pos

prop_good_moves :: Position -> Bool
prop_good_moves p = (normal p >= total p - 1) == (null $ good_moves p)

is_solution :: Position -> Bool
is_solution p = normal  p >= total p - 1

depth_unknown :: Position -> Int
depth_unknown = depth_with is_solution

case_depth_unknown_4  = 2 @=? depth_unknown (start_position 4)
case_depth_unknown_13 = 3 @=? depth_unknown (start_position 13)
case_depth_unknown_14 = 4 @=? depth_unknown (start_position 14)

is_perfect_solution :: Position -> Bool
is_perfect_solution p = normal p >= total p - 1 && unknown p == 0

depth_perfect :: Position -> Int
depth_perfect = depth_with is_perfect_solution

case_depth_perfect_4  = 3 @=? depth_perfect (start_position 4)
case_depth_perfect_12 = 3 @=? depth_perfect (start_position 12)
case_depth_perfect_13 = 4 @=? depth_perfect (start_position 13)

memoized :: Position -> (Position -> a) -> (Position -> a)
memoized pos func = \p -> fromJust $ M.lookup p table
  where
    mkpos u h l n = Position {unknown = u, heavy = h, light = l, normal = n}
    allpos = [mkpos u h l n | u <- [0..(unknown pos)            ]
                            , h <- [0..(unknown pos + heavy pos)]
                            , l <- [0..(unknown pos + light pos)]
                            , n <- [0..(total pos)              ]
                            ]
    table = M.fromList $ map (\p -> (p, func p)) allpos

calc_depth :: (Position -> Maybe Int)
           -> (Position -> Bool)
           -> Position
           -> Maybe Int
calc_depth func cond p
  | cond p    = Just 0
  | otherwise = liftM (+1)
                $ minimumBy depth_ord
                $ map (maximumBy depth_ord . map func . apply_move p)
                $ good_moves p
  where
    depth_ord Nothing  Nothing  = EQ
    depth_ord _        Nothing  = LT
    depth_ord Nothing  _        = GT
    depth_ord (Just a) (Just b) = a `compare` b

depth_with :: (Position -> Bool) -> Position ->  Int
depth_with cond pos = fromJust $ depth pos
  where
    depth = memoized pos $ calc_depth depth cond

data Tree = Branch { move     :: Move
                   , position :: Position
                   , left     :: Tree
                   , equal    :: Tree
                   , right    :: Tree
                   }
          | Node Position

solution_with :: (Position -> Bool) -> Position -> Tree
solution_with cond pos
  | cond pos  = Node pos
  | otherwise = Branch { move     = best_move
                       , position = pos
                       , left     = solution_with cond $ apply_result Left  pos best_move
                       , equal    = solution_with cond $ apply_result Equal pos best_move
                       , right    = solution_with cond $ apply_result Right pos best_move
                       }
  where
    mydepth       = memoized pos $ calc_depth mydepth cond
    best_move     = fst $ minimumBy moves_ord $ map add_depth $ good_moves pos
    add_depth m   = (m, maximum $ map mydepth $ apply_move pos m)
    moves_ord a b = snd a `compare` snd b

solution_unknown :: Position -> Tree
solution_unknown = solution_with is_solution

solution_perfect :: Position -> Tree
solution_perfect = solution_with is_perfect_solution

pretty_print :: Tree -> String
pretty_print = show . tree_doc
  where
    tree_doc Branch { move     = Move ml mr
                    , position = p
                    , left     = l
                    , equal    = e
                    , right    = r
                    }
      = text (show p ++ ": " ++ show ml ++ " _|_ " ++ show mr)
        $$ text "Left: " <+> tree_doc l
        $$ text "Equal:" <+> tree_doc e
        $$ text "Right:" <+> tree_doc r
    tree_doc (Node pos)
      = text (show pos)

instance Show Tree where
  show t = pretty_print t
