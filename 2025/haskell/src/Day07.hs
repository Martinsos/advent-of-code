module Day07 where

import Data.Either (fromRight)
import Data.Void
import Paths_aoc (getDataFileName)
import Test.Hspec (describe, hspec, shouldBe, specify)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

day07 :: IO (Int, Int)
day07 = solve <$> (getDataFileName "day07-input.txt" >>= readFile)

solve :: String -> (Int, Int)
solve input = const (42, 314) $ parseInput input

type Parser = P.Parsec Void String

parseInput :: String -> ()
parseInput = fromRight (error "parse error") . P.parse inputP ""
  where
    inputP :: Parser ()
    inputP = do
      _ <- P.string "test"
      pure ()

------------------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  describe "solve" $ do
    specify "dummy test case" $
      solve "test" `shouldBe` (42, 314)

--  specify "day07" $ do
--    day07 >>= (`shouldBe` "actual solution")
