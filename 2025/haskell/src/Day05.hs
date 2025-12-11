module Day05 where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (intercalate, sort)
import Data.Void
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (id)

day05 :: IO (Int, Int)
day05 = solve <$> (getDataFileName "day05-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input = (length $ filterIdsInRanges ranges ids, undefined)
  where
    (ranges, ids) = parseInput input

type Parser = P.Parsec Void String

parseInput :: String -> ([IdRange], [Id])
parseInput = fromRight (error "parse error") . P.parse inputP ""
  where
    inputP = do
      ranges <- P.some $ rangeP <* P.eol
      _ <- P.eol
      ids <- P.some $ idP <* (void P.eol P.<|> P.eof)
      pure (ranges, ids)

    rangeP :: Parser IdRange
    rangeP = IdRange <$> idP <* P.char '-' <*> idP

    idP :: Parser Id
    idP = L.decimal

type Id = Int

data IdRange = IdRange
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq)

instance Ord IdRange where
  compare r1 r2 = compare r1.start r2.start <> compare r1.end r2.end

{- ORMOLU_DISABLE -}
filterIdsInRanges :: [IdRange] -> [Id] -> [Id]
filterIdsInRanges ranges ids = go (sortAndMergeIdRanges ranges) (sort ids)
  where
    go :: [IdRange] -> [Id] -> [Id]
    go [] _ = []
    go _ [] = []
    go (r : rangesRest) (id : idsRest)
      | id < r.start   =      go (r : rangesRest)       idsRest
      | id `inRange` r = id : go (r : rangesRest)       idsRest
      | otherwise      =      go      rangesRest  (id : idsRest)
{- ORMOLU_ENABLE -}

sortAndMergeIdRanges :: [IdRange] -> [IdRange]
sortAndMergeIdRanges ranges = mergeSortedRanges $ sort ranges
  where
    mergeSortedRanges :: [IdRange] -> [IdRange]
    mergeSortedRanges (r1 : r2 : rRest) =
      if r2.start `inRange` r1
        then mergeSortedRanges $ IdRange r1.start (max r1.end r2.end) : rRest
        else r1 : mergeSortedRanges (r2 : rRest)
    mergeSortedRanges rs = rs

inRange :: Id -> IdRange -> Bool
inRange id r = r.start <= id && id <= r.end

------------------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let exampleInputText =
        intercalate
          "\n"
          [ "3-5",
            "10-14",
            "16-20",
            "12-18",
            "",
            "1",
            "5",
            "8",
            "11",
            "17",
            "32"
          ]
  let exampleInputRanges = uncurry IdRange <$> [(3, 5), (10, 14), (16, 20), (12, 18)]
  let exampleInputIds = [1, 5, 8, 11, 17, 32]

  describe "parseInput" $ do
    specify "example input" $ do
      parseInput exampleInputText `shouldBe` (exampleInputRanges, exampleInputIds)

  describe "sortAndMergeIdRanges" $ do
    specify "example input" $
      sortAndMergeIdRanges exampleInputRanges `shouldBe` (uncurry IdRange <$> [(3, 5), (10, 20)])

  describe "filterIdsInRanges" $ do
    specify "example input" $ do
      filterIdsInRanges exampleInputRanges exampleInputIds `shouldBe` [5, 11, 17]

  describe "solve" $ do
    specify "example input" $
      fst (solve exampleInputText) `shouldBe` 3

  specify "day05" $ do
    day05 >>= (`shouldBe` 701) . fst
