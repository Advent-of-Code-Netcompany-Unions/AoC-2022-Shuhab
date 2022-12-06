{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( transpose )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( count,
      sepBy,
      sepBy1,
      decimal,
      letter,
      space,
      endOfLine,
      takeTill,
      Parser )
import Data.Void
import Control.Applicative ( Alternative((<|>)), liftA3 )
import Data.Functor ( ($>), (<&>) )
import Data.Foldable
import Data.Function
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = let
  crate = ("[" *> letter <* "]" <&> Just) <|> (count 3 space $> Nothing)
  line = liftA3 (,,) ("move " *> decimal) (" from " *> decimal) (" to " *> decimal)
  in do
  crates <- Map.fromList . zip [1..] . fmap catMaybes . transpose <$> crate `sepBy` " " `sepBy` endOfLine
  takeTill (== 'm')
  lines <- line `sepBy1` endOfLine
  pure (crates, lines)        

------------ TYPES ------------
type Input = (Map Int [Char], [(Int, Int, Int)]) 

type OutputA = String

type OutputB = String

------------ PART A ------------
doMoves :: (Foldable t, Ord k) => ([a] -> [a]) -> Map k [a] -> t (Int, k, k) -> Map k [a]
doMoves f = foldl' \piles (n,x,y) ->
  let p = f $ take n $ piles Map.! x
  in piles & Map.adjust (drop n) x & Map.adjust (p ++) y

partA :: Input -> OutputA
partA = map head . Map.elems . uncurry (doMoves reverse)

------------ PART B ------------
partB :: Input -> OutputB
partB = map head . Map.elems . uncurry (doMoves id)
