module Test.MySolutions where

import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, filter, head, length, null, tail, (..), (:))
import Data.Foldable (foldl)
import Data.Int (pow)
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, size, root)
import Data.String.Utils (endsWith)
import Data.Tuple (Tuple(..), snd)
import Test.Examples (allFiles, factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = n == 0 || (not $ isEven $ n - 1)

countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else
    let
      headIsEven = isEven $ fromMaybe 1 $ head arr
    in
      let
        currentValue = if headIsEven then 1 else 0
      in
        currentValue + (countEven $ fromMaybe [] $ tail arr)

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\x -> x >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n = if n <= 1 then false else length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

factorize :: Int -> Array Int
factorize n =
  if n <= 1 then
    [ 1 ]
  else
    factorize' n 2 []

factorize' :: Int -> Int -> Array Int -> Array Int
factorize' n i acc =
  if n == 1 then
    acc
  else
    if n `mod` i == 0 then
      factorize' (n / i) i (i : acc)
    else
      factorize' n (i + 1) acc

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec n =
  if n < 0 then
    0
  else
    fibTailRec' n 0 0 1

fibTailRec' :: Int -> Int -> Int -> Int -> Int
fibTailRec' n count a b =
  if n == count then
    b
  else
    fibTailRec' n (count + 1) b (a + b)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> x : xs) []

onlyFiles :: Path -> Array Path
onlyFiles file =
  if isDirectory file then
    concatMap onlyFiles (ls file)
  else
    [ file ]

maxInt :: Int
maxInt = (pow 2 31 - 1)

largestSmallest :: Path → Array (Tuple String Int)
largestSmallest = largestSmallestTake1

largestSmallestTake1 :: Path → Array (Tuple String Int)
largestSmallestTake1 file =
  let
    files = allFiles file
  in
    let
      defaultLargest = Tuple "" 0
    in
      let
        largest =
          foldl
            ( \largest' file' ->
                let
                  filesize = fromMaybe 0 $ size file'
                in
                  if (filesize > snd largest') then
                    Tuple (filename file') filesize
                  else
                    largest'
            )
            defaultLargest
            files
      in
        let
          defaultSmallest = Tuple "" maxInt
        in
          let
            smallest =
              foldl
                ( \smallest' file' ->
                    let
                      filesize = fromMaybe maxInt $ size file'
                    in
                      if (filesize < snd smallest') then
                        Tuple (filename file') filesize
                      else
                        smallest'
                )
                defaultSmallest
                files
          in
            [ largest, smallest ]

-- whereIs :: String -> Maybe Path
-- whereIs name =
--   head $ allFiles root
--     >>= \path ->
--         ls path
--           >>= \child ->
--               guard (filename child == name)
--                 >>= \_ -> pure path
whereIs :: String -> Maybe String
whereIs name =
  head
    $ do
        path <- allFiles root
        child <- (ls path)
        guard $ endsWith name $ filename child
        pure (filename path)
