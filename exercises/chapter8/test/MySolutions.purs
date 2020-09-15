module Test.MySolutions where

import Prelude
import Data.Array (sort, head, tail)
import Data.Maybe (Maybe)
import Data.Foldable (foldM)
import Data.List (List(..), (:))

-- Note to reader: Add your solutions to this file
third :: forall t3. Array t3 -> Maybe t3
third arr = do
  xs <- tail arr
  ys <- tail xs
  head ys

possibleSums :: Array Int -> Array Int
possibleSums coins = sort $ foldM (\acc c -> [ acc, acc + c ]) 0 coins

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil

filterM f (x : xs) = do
  keep <- f x
  rest <- filterM f xs
  pure if keep then x : rest else rest
