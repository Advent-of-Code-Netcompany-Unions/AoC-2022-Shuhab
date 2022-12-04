module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
ranges :: Parser (Set Int, Set Int) 
ranges = do
    s1 <- decimal
    char '-'
    e1 <- decimal
    char ','
    s2 <- decimal
    char '-'
    e2 <- decimal
    return (Set.fromList [s1..e1], Set.fromList [s2..e2])

inputParser :: Parser Input
inputParser = ranges `sepBy` endOfLine

------------ TYPES ------------
type Input = [(Set Int, Set Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter id . fmap (\(s1, s2) -> Set.isSubsetOf s1 s2 || Set.isSubsetOf s2 s1)

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter (not . null) . fmap (uncurry Set.intersection)
