module Day03 where

import Data.Either (fromRight)
import Data.Void
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

day03 :: IO (Integer, Integer)
day03 = solve <$> (getDataFileName "day03-input.txt" >>= readFile)

solve :: String -> (Integer, Integer)
solve input = const (42, 314) $ parseInput input

type Parser = P.Parsec Void String

parseInput :: String -> ()
parseInput = fromRight (error "parse error") . P.parse testP ""
  where
    testP :: Parser ()
    testP = do
      _ <- P.string "test"
      pure ()

tests :: IO ()
tests = hspec $ do
  describe "solve" $ do
    specify "dummy test case" $
      solve "test" `shouldBe` (42, 314)

--  specify "day03" $ do
--    day03 >>= (`shouldBe` "actual solution")
