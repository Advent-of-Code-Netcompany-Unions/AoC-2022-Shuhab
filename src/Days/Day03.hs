module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import Data.Char
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
inputParser = many1 letter `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
priority :: Char -> Int
priority x = 
    if
        | isAlpha x && isLower x -> ord x - 96 -- a-z is 1 to 26
        | isAlpha x && isUpper x -> ord x - 38 -- A-Z is 27 to 52
        | otherwise -> error "Illegal input character"

partA :: Input -> OutputA
partA = sum . fmap (priority . uncurry shared . (\s -> splitAt (length s `div` 2) s))
    where
        shared l r = Set.elemAt 0 $ Set.fromList l `Set.intersection` Set.fromList r
             

------------ PART B ------------
partB :: Input -> OutputB
partB = findBadges
    where
        findBadges [] = 0
        findBadges (x:y:z:ss) = 
            findBadges ss 
            + (priority . Set.elemAt 0 . foldl1' Set.intersection $ 
                [Set.fromList x, Set.fromList y, Set.fromList z])
