module Test.MySolutions where

import Data.Maybe (Maybe(..))
import Data.Picture (Point(..), Shape(..))
import Math (pi)
import Prelude (negate, otherwise, (*), (+), (-), (/), (<), (==))

--
factorial :: Int -> Int
factorial 0 = 1

factorial n = n * factorial (n - 1)

--
binomial :: Int -> Int -> Int
binomial _ 0 = 1

binomial 0 _ = 0

binomial n k
  | n < k = 0
  | otherwise = (factorial n) / (factorial k * factorial (n - k))

--
pascal :: Int -> Int -> Int
pascal _ 0 = 1

pascal 0 _ = 0

pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

--
-- sameCity :: Person -> Person -> Boolean
sameCity ::
  forall r1 r1' r2 r2'.
  { address :: { city :: String | r1' } | r1 } ->
  { address :: { city :: String | r2' } | r2 } ->
  Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } = city1 == city2

--
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x

fromSingleton default _ = default

--
origin :: Point
origin = Point { x: 0.0, y: 0.0 }

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

--
addPoints :: Point -> Point -> Point
addPoints (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 }) = Point { x: x1 + x2, y: y1 + y2 }

scalePoint :: Number -> Point -> Point
scalePoint s (Point { x, y }) = Point { x: s * x, y: s * y }

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ size) = Circle origin (2.0 * size)

doubleScaleAndCenter (Rectangle _ n m) = Rectangle origin (2.0 * n) (2.0 * m)

doubleScaleAndCenter (Line p1@(Point { x: x1, y: y1 }) p2@(Point { x: x2, y: y2 })) = Line p1' p2'
  where
  midpoint = Point { x: (x1 + x2) / 2.0, y: (y1 + y2) / 2.0 }

  centeredP1 = addPoints p1 (scalePoint (-1.0) midpoint)

  centeredP2 = addPoints p2 (scalePoint (-1.0) midpoint)

  p1' = scalePoint 2.0 centeredP1

  p2' = scalePoint 2.0 centeredP2

doubleScaleAndCenter (Text _ text) = Text origin text

doubleScaleAndCenter c@(Clipped _ _ _ _) = c

--
shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text

shapeText _ = Nothing

--
area :: Shape -> Number
area (Circle _ r) = pi * r * r

area (Rectangle _ width height) = width * height

area (Line _ _) = 0.0

area (Text _ _) = 0.0

area (Clipped _ _ _ _) = 0.0
