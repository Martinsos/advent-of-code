module Day02 where

import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.List (foldl1', sort)
import Data.Void
import GHC.List (iterate')
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, it, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Prelude hiding (id)

day02 :: IO (Integer, Integer)
day02 = solve <$> (getDataFileName "day02-input.txt" >>= readFile)

solve :: String -> (Integer, Integer)
solve input =
  let idRanges = parseIdRanges input
      sumOfAllInvalidIdsWithDoublePattern =
        sum $
          concatMap (\r -> calcInvalidIdsWithPatternCountInRange r 2) idRanges
      sumOfAllInvalidIds =
        sum $
          concatMap
            (\r -> nubOrd $ concatMap (calcInvalidIdsWithPatternCountInRange r) [2 .. lengthIntegral (snd r)])
            idRanges
   in (sumOfAllInvalidIdsWithDoublePattern, sumOfAllInvalidIds)

type Id = Integer

type IdChunk = Integer

type Parser = P.Parsec Void String

parseIdRanges :: String -> [(Id, Id)]
parseIdRanges = fromRight (error "parse error") . P.parse rangesP ""
  where
    rangesP :: Parser [(Id, Id)]
    rangesP = rangeP `P.sepBy` P.char ','
    rangeP :: Parser (Id, Id)
    rangeP = (,) <$> P.decimal <* P.char '-' <*> P.decimal

calcInvalidIdsWithPatternCountInRange :: (Id, Id) -> Int -> [Id]
calcInvalidIdsWithPatternCountInRange (startId, endId) patternCount =
  takeWhile (<= endId) $ map (replicateIntegral patternCount) $ iterate' (+ 1) $ patternOfFirstInvalidId patternCount startId

patternOfFirstInvalidId :: Int -> Id -> IdChunk
patternOfFirstInvalidId patternCount id =
  if idLength `mod` patternCount == 0
    then
      let pattern = digitsToIntegral $ take (idLength `div` patternCount) idDigits
       in if replicateIntegral patternCount pattern >= id
            then pattern
            else pattern + 1
    else digitsToIntegral $ 1 : replicate (idLength `div` patternCount) 0
  where
    idDigits = integralToDigits id
    idLength = length idDigits

integralToDigits :: (Integral a) => a -> [a]
integralToDigits x = reverse $ go x
  where
    go y | y < 10 = [y]
    go y = (y `mod` 10) : go (y `div` 10)

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral = foldl1' (\x d -> x * 10 + d)

lengthIntegral :: (Integral a) => a -> Int
lengthIntegral = length . integralToDigits

replicateIntegral :: (Integral a) => Int -> a -> a
replicateIntegral n = digitsToIntegral . concat . replicate n . integralToDigits

tests :: IO ()
tests = hspec $ do
  specify "numToDigits" $ do
    integralToDigits (12345 :: Integer) `shouldBe` [1, 2, 3, 4, 5]
    integralToDigits (12345 :: Int) `shouldBe` [1, 2, 3, 4, 5]
    integralToDigits (0 :: Int) `shouldBe` [0]
    integralToDigits (9 :: Int) `shouldBe` [9]
    integralToDigits (10 :: Int) `shouldBe` [1, 0]
  specify "digitsToIntegral" $ do
    digitsToIntegral [1, 2, 3, 4, 5] `shouldBe` (12345 :: Integer)
    digitsToIntegral [1, 2, 3, 4, 5] `shouldBe` (12345 :: Int)
    digitsToIntegral [0] `shouldBe` (0 :: Int)
    digitsToIntegral [9] `shouldBe` (9 :: Int)
    digitsToIntegral [1, 0] `shouldBe` (10 :: Int)
  specify "patternOfFirstInvalid" $ do
    patternOfFirstInvalidId 2 1212 `shouldBe` 12
    patternOfFirstInvalidId 3 1212 `shouldBe` 10
    patternOfFirstInvalidId 2 1211 `shouldBe` 12
    patternOfFirstInvalidId 2 121 `shouldBe` 10
    patternOfFirstInvalidId 2 12345 `shouldBe` 100
    patternOfFirstInvalidId 3 123456 `shouldBe` 13
  specify "replicateIntegral" $ do
    replicateIntegral 1 (12 :: Integer) `shouldBe` 12
    replicateIntegral 3 (123 :: Integer) `shouldBe` 123123123
  describe "calcInvalidIdsWithPatternCountInRange" $ do
    let (range, counts) ~> expectedIdLists =
          describe ("for range " <> show range) $
            sequence_ $
              ( \(c, ids) ->
                  specify ("for pattern count " <> show c <> " -> " <> show ids) $
                    sort (calcInvalidIdsWithPatternCountInRange range c) `shouldBe` sort ids
              )
                <$> zip counts expectedIdLists
    ((11, 22), [2, 3]) ~> [[11, 22], []]
    ((95, 115), [2, 3]) ~> [[99], [111]]
    ((998, 1012), [2, 3]) ~> [[1010], [999]]
    ((1188511880, 1188511890), [2 .. 10]) ~> ([[1188511885]] <> replicate 9 [])
    ((222220, 222224), [2 .. 6]) ~> [[222222], [222222], [], [], [222222]]
    ((1698522, 1698528), [2 .. 7]) ~> replicate 7 []
    ((446443, 446449), [2 .. 6]) ~> ([[446446]] <> replicate 5 [])
    ((38593856, 38593862), [2 .. 8]) ~> ([[38593859]] <> replicate 7 [])
    ((565653, 565659), [2 .. 6]) ~> ([[], [565656]] <> replicate 4 [])
    ((824824821, 824824827), [2 .. 10]) ~> ([[], [824824824]] <> replicate 7 [])
    ((2121212118, 2121212124), [2 .. 10]) ~> (replicate 3 [] <> [[2121212121]] <> replicate 5 [])
  specify "parseIdRanges" $ do
    parseIdRanges "1-2,33-456" `shouldBe` [(1, 2), (33, 456)]
  describe "solve" $ do
    specify "for simple case with duplicates" $
      solve "222220-222224" `shouldBe` (222222, 222222)
    it "should work for the example input" $ do
      solve "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" `shouldBe` (1227775554, 4174379265)
  specify "day02" $ do
    day02 >>= (`shouldBe` (31839939622, 41662374059))
