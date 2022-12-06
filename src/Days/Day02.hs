module Days.Day02 (runDay) where

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
    ( sepBy, space, char, endOfLine, Parser )
import Data.Void ()
import Data.Functor ( ($>) )
import Control.Applicative ( Alternative((<|>)) )
import Util.Parsers ( around )
import System.Console.ANSI (saveCursorCode)
import Data.Char ( ord )
import Data.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ((opponent <|> you) `around` space) `sepBy` endOfLine
  where
    opponent = (\o -> ord o - ord 'A') <$> (char 'A' <|> char 'B' <|> char 'C') 
    you = (\y -> ord y - ord 'X') <$> (char 'X' <|> char 'Y' <|> char 'Z')

------------ TYPES ------------

type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

partA :: Input -> OutputA
partA = sum . L.map (\(o, y) -> 3 * ((y - o + 1) `mod` 3) + (y + 1))

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . L.map (\(o, y) -> (y * 3) + ((o + y + 2) `mod` 3) + 1)