module Day02 where

import Data.Either (fromRight)
import Data.List (foldl1')
import Data.Void
import GHC.List (iterate')
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, it, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Prelude hiding (id)

day02 :: IO Integer
day02 = solve <$> (getDataFileName "day02-input.txt" >>= readFile)

solve :: String -> Integer
solve input =
  let idRanges = parseIdRanges input
      sumOfAllInvalidIds = sum $ concatMap calcInvalidIdsInRange idRanges
   in sumOfAllInvalidIds

type Id = Integer

type Parser = P.Parsec Void String

parseIdRanges :: String -> [(Id, Id)]
parseIdRanges = fromRight (error "parse error") . P.parse rangesP ""
  where
    rangesP :: Parser [(Id, Id)]
    rangesP = rangeP `P.sepBy` P.char ','
    rangeP :: Parser (Id, Id)
    rangeP = (,) <$> P.decimal <* P.char '-' <*> P.decimal

calcInvalidIdsInRange :: (Id, Id) -> [Id]
calcInvalidIdsInRange (startId, endId) =
  takeWhile (<= endId) $ map doubleTheDigits $ iterate' (+ 1) $ halfOfFirstInvalidIdGte startId

halfOfFirstInvalidIdGte :: Id -> Id
halfOfFirstInvalidIdGte id =
  let idDigits = integralToDigits id
   in case takeHalf idDigits of
        Nothing -> digitsToIntegral $ 1 : replicate (length idDigits `div` 2) 0
        Just halfDigits ->
          let half = digitsToIntegral halfDigits
           in half + (if doubleTheDigits half < id then 1 else 0)

integralToDigits :: (Integral a) => a -> [a]
integralToDigits x = reverse $ go x
  where
    go y | y < 10 = [y]
    go y = (y `mod` 10) : go (y `div` 10)

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral = foldl1' (\x d -> x * 10 + d)

takeHalf :: [a] -> Maybe [a]
takeHalf xs = let len = length xs in if even len then Just (take (len `div` 2) xs) else Nothing

doubleTheDigits :: (Integral a) => a -> a
doubleTheDigits = digitsToIntegral . (\ds -> ds <> ds) . integralToDigits

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
  specify "takeHalf" $ do
    takeHalf "test" `shouldBe` Just "te"
    takeHalf "tes" `shouldBe` Nothing
    takeHalf "" `shouldBe` Just ""
  specify "halfOfFirstInvalidIdGte" $ do
    halfOfFirstInvalidIdGte 1212 `shouldBe` 12
    halfOfFirstInvalidIdGte 1211 `shouldBe` 12
    halfOfFirstInvalidIdGte 121 `shouldBe` 10
    halfOfFirstInvalidIdGte 12345 `shouldBe` 100
  specify "doubleTheDigits" $ do
    doubleTheDigits (12 :: Integer) `shouldBe` 1212
    doubleTheDigits (11 :: Integer) `shouldBe` 1111
  specify "calcInvalidIdsInRange" $ do
    calcInvalidIdsInRange (11, 22) `shouldBe` [11, 22]
    calcInvalidIdsInRange (13, 23) `shouldBe` [22]
    calcInvalidIdsInRange (95, 115) `shouldBe` [99]
    calcInvalidIdsInRange (998, 1012) `shouldBe` [1010]
    calcInvalidIdsInRange (1188511880, 1188511890) `shouldBe` [1188511885]
    calcInvalidIdsInRange (222220, 222224) `shouldBe` [222222]
    calcInvalidIdsInRange (1698522, 1698528) `shouldBe` []
    calcInvalidIdsInRange (446443, 446449) `shouldBe` [446446]
    calcInvalidIdsInRange (38593856, 38593862) `shouldBe` [38593859]
    calcInvalidIdsInRange (565653, 565659) `shouldBe` []
    calcInvalidIdsInRange (824824821, 824824827) `shouldBe` []
    calcInvalidIdsInRange (2121212118, 2121212124) `shouldBe` []
  specify "parseIdRanges" $ do
    parseIdRanges "1-2,33-456" `shouldBe` [(1, 2), (33, 456)]
  describe "solve" $ do
    it "should work for the example input" $ do
      solve "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" `shouldBe` 1227775554
