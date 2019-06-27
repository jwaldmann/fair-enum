-- | https://github.com/rudymatela/leancheck/issues/14#issuecomment-504990199

{-# language LambdaCase #-}

import Numeric.Natural
import Data.Ratio
import Data.List (inits, tails)
import Control.Monad (guard, forM_, replicateM)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Q
import System.Environment

main = getArgs >>= \ case
  [ "tier", w, h] -> solve $ tier (read w) (read h)
  [ "tuples", w, h] -> solve $ tuples (read w) (read h)
  [ "perms", w ] -> solve $ perms (read w)

test = do
  solve $ perms 4

perms w = permutations [1..w]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = do
  (pre,this:post) <- zip (inits xs)(tails xs)
  (this :) <$> permutations (pre <> post)

tuples :: Natural -> Natural -> [[Natural]]
tuples w h = replicateM (fromIntegral w) [1..h]

-- | all lists of natural numbers of length w and sum h
tier :: Natural -> Natural -> [[Natural]]
tier 1 height = [[height]]
tier width height | width > 1 = do
  x <- [0..height]
  xs <- tier (width-1) (height-x)
  return $ x : xs

type Dist a w = M.Map a w

dist :: (Num w, Ord a) => [a] -> Dist a w
dist xs = M.fromListWith (+) $ zip xs $ repeat 1


data State a w = State
  { distributions :: ! [Dist a w]
  , total :: ! Int
  , history :: ! [[a]]
  } deriving Show

state0 :: Natural -> State a w
state0 w = State
  { distributions = replicate (fromIntegral w) M.empty , total = 0, history = [] }

state :: ( Ord a, Num w) => Natural -> [[a]] -> State a w
state w xs = foldl (flip push) (state0 w) xs

push :: (Ord a, Num w) => [a] -> State a w -> State a w
push x s = s
  { distributions = zipWith (\ x d -> M.insertWith (+) x 1 d) x (distributions s )
  , total = 1 + total s
  , history = x : history s
  }
  
distance :: (Ord a, Fractional w, Ord w) => State a w -> State a w -> w
distance s t | total s <= total t =
  let ts = fromIntegral (total s)
      tt = fromIntegral (total t)
  in  maximum $ do
        (d,e) <- zip (distributions s) (distributions t)
	let delta = maximum 
               $ M.map abs
	       $ M.unionWith (+) (M.map (/ ts) d)
	       $ M.map (negate . (/ tt)) e
	return $ delta * ts

solve ts = do
  mapM_ print $ map (\(v,s) -> (v, reverse $ history s)) $ solutions ts

solutions :: [[Natural]] -> [ (Rational, State Natural Rational) ]
solutions ts =
  let w = fromIntegral $ length $ head ts
      goal = state w ts
      eval :: State Natural Rational -> Rational
      eval s = distance s goal
      go_below
        :: Rational -> State Natural Rational -> Q.Seq [Natural]
        -> [ State Natural Rational ]
      go_below b s ps =
        if null ps
	then return s
	else do
	  i <- [ 0 .. length ps - 1 ]
	  let x = Q.index ps i 
	      s' = push x s
	  guard $ eval s' < b
	  go_below b s' $ Q.deleteAt i ps
      evals ts = maximum $ do
        t <- tail $ scanl (\ s x -> push x s) (state0 w) ts
	return $ eval t
      pack s = (evals $ reverse $ history s, s)
      top (bound,best) = (bound,best) : case go_below bound (state0 w) $ Q.fromList ts of
        [] -> []
	s : _ -> top $ pack s
  in  top $ pack goal


  
