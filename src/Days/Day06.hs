module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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
inputParser :: Parser Input
inputParser = many1 letter

------------ TYPES ------------
type Input = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
findSignal :: Int -> Input -> OutputA
findSignal n = (+n) . head . elemIndices n . map (length . nub . L.take n) . tails

findStartOfPacketMarker :: Input -> OutputA
findStartOfPacketMarker = findSignal 4

partA :: Input -> OutputA
partA = findStartOfPacketMarker

------------ PART B ------------
findStartOfMessageMarker :: Input -> OutputA
findStartOfMessageMarker = findSignal 14

partB :: Input -> OutputB
partB = findStartOfMessageMarker 
