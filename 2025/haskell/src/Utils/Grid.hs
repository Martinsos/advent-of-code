module Utils.Grid
  ( Grid (..),
    (@!?),
    fromList,
    tests,
  )
where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec (describe, hspec, shouldBe, specify)

data Grid a = Grid
  { vector :: !(Vector a),
    width :: !Int,
    height :: !Int
  }
  deriving (Show, Eq)

instance Functor Grid where
  fmap f g = g {vector = fmap f g.vector}

fromList :: Int -> [a] -> Grid a
fromList width xs =
  Grid
    { vector = V.fromList xs,
      width = width,
      height = length xs `div` width
    }

infixl 9 @!?

(@!?) :: Grid a -> (Int, Int) -> Maybe a
(@!?) grid (r, c) | r < 0 || r >= grid.height || c < 0 || c >= grid.width = Nothing
(@!?) grid (r, c) = grid.vector V.!? (r * grid.width + c)

--------------------------------------------------------------------------------

tests :: IO ()
tests = hspec $ do
  let mockGrid = fromList 3 ("..@@@@@.@.@." :: String)
  specify "fromList" $ do
    fromList 3 ("..@@@@@.@.@." :: String)
      `shouldBe` Grid
        { vector = V.fromList "..@@@@@.@.@.",
          width = 3,
          height = 4
        }
  describe "@!?" $ do
    specify "for simple cases" $ do
      fromList 2 "@..." @!? (0, 0) `shouldBe` Just '@'
      fromList 2 ".@.." @!? (0, 1) `shouldBe` Just '@'
      fromList 2 "..@." @!? (1, 0) `shouldBe` Just '@'
      fromList 2 "...@" @!? (1, 1) `shouldBe` Just '@'
      fromList 2 "...@" @!? (1, -1) `shouldBe` Nothing
    specify "for mock grid" $ do
      fromJust . (mockGrid @!?)
        <$> [(r, c) | r <- [0 .. (mockGrid.height - 1)], c <- [0 .. (mockGrid.width - 1)]]
        `shouldBe` V.toList mockGrid.vector
