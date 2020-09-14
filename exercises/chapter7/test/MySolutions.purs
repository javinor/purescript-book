module Test.MySolutions where

import Prelude

import Control.Apply (lift2, lift3)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, arrayNonEmpty, matches, nonEmpty, validateAddress, validatePhoneNumber)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)
import Partial.Unsafe (unsafePartial)

-- Note to reader: Add your solutions to this file
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

--
addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

--
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing

combineMaybe (Just fa) = Just <$> fa

--
stateRegex :: Regex
stateRegex =
  unsafePartial case regex "^[a-zA-Z][a-zA-Z]$" noFlags of
    Right r -> r

nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial case regex "[^\\s]" noFlags of
    Right r -> r

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved adr = ado
  street <- (matches "Street" nonEmptyRegex adr.street *> pure adr.street)
  city <- (matches "City" nonEmptyRegex adr.city *> pure adr.city)
  state <- (matches "State" stateRegex adr.state *> pure adr.state)
  in address street city state

--
data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

instance treeEq :: Eq a => Eq (Tree a) where
  eq Leaf Leaf = true
  eq Leaf (Branch _ _ _) = false
  eq (Branch _ _ _) Leaf = false
  eq (Branch x_left x x_right) (Branch y_left y y_right) = x == y && eq x_left y_left && eq x_right y_right

instance treeShow :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch left x right) = "(Branch " <> show left <> " " <> show x <> " " <> show right <> ")"

instance treeFunctor :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch left x right) = Branch (map f left) (f x) (map f right)

instance treeFoldable :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch left x right) =
    let
      acc_left = foldl f acc left

      acc_mid = f acc_left x

      acc_right = foldl f acc_mid right
    in
      acc_right
  foldr _ acc Leaf = acc
  foldr f acc (Branch left x right) =
    let
      acc_right = foldr f acc right

      acc_mid = f x acc_right

      acc_left = foldr f acc_mid left
    in
      acc_left
  foldMap f Leaf = mempty
  foldMap f (Branch left x right) = foldMap f left <> f x <> foldMap f right

instance treeTraverse :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch left x right) = lift3 Branch (traverse f left) (f x) (traverse f right)
  sequence tma = traverse identity tma

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf

traversePreOrder f (Branch left x right) = ado
  x' <- f x
  left' <- traversePreOrder f left
  right' <- traversePreOrder f right
  in Branch left' x' right'

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch left x right) = ado
  left' <- traversePostOrder f left
  right' <- traversePostOrder f right
  x' <- f x
  in Branch left' x' right'

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p = ado
  firstName <- (nonEmpty "First Name" p.firstName *> pure p.firstName)
  lastName <- (nonEmpty "Last Name" p.lastName *> pure p.lastName)
  homeAddress <- traverse validateAddress p.homeAddress
  phones <- (arrayNonEmpty "Phone Numbers" p.phones *> traverse validatePhoneNumber p.phones)
  in { firstName, lastName, homeAddress, phones }

sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse tma = traverse identity tma

traverseUsingSequence  :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence fab ta = sequence $ fab <$> ta