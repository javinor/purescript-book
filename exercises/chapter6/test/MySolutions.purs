module Test.MySolutions where

import Prelude

import Data.Array (nubByEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Hashable (class Hashable, hashCode, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)

-- Note to reader: Add your solutions to this file
newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

instance showComplex :: Show Complex where
  show (Complex { real: re, imaginary: im }) = show re <> (if im >= 0.0 then "+" else "") <> show im <> "i"

--
data NonEmpty a
  = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = "NonEmpty " <> show x <> " " <> show xs

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [ y ] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

data Extended a
  = Finite a
  | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = eq x y
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite x) (Finite y) = x `compare` y

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f empty (NonEmpty x xs) = foldl f (f empty x) xs
  foldr f empty (NonEmpty x xs) = f x (foldr f empty xs)
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl func empty (OneMore a fa) = foldl func (func empty a) fa
  foldr func empty (OneMore a fa) = func a (foldr func empty fa)
  foldMap func (OneMore a fa) = func a <> foldMap func fa

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just x -> x

newtype Multiply
  = Multiply Int

derive instance eqMultiply :: Eq Multiply

instance showMultiply :: Show Multiply where
  show (Multiply n) = "Multiply " <> show n

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

class
  Monoid m <= Action m a where
  act :: m -> a -> a

instance actionMultiplyInt :: Action Multiply Int where
  act (Multiply 1) n = n
  act (Multiply m) n = m * n

instance repeatAction :: Action Multiply String where
  act (Multiply n) str = power str n

instance actionArray :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

newtype Self m
  = Self m

derive instance eqSelf :: Eq a => Eq (Self a)

instance showSelf :: Show a => Show (Self a) where
  show (Self a) = "Show " <> show a

instance actionSelf :: Monoid m => Action m (Self m) where
  act m (Self m') = Self (m <> m')

instance semigroupSelf :: Semigroup a => Semigroup (Self a) where
  append (Self x) (Self y) = Self (x <> y)

instance monoidSelf :: Monoid a => Monoid (Self a) where
  mempty = Self mempty

instance actionSelfMultiply :: Action (Self Multiply) Int where
  act (Self (Multiply m)) n = m * n

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr = arr /= nubByEq eq' arr
  where
  eq' x y = hashEqual x y && eq x y

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hashCode (n `mod` 12)