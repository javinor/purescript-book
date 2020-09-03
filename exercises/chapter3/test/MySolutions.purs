module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry, findEntry)
import Data.List (filter, head, nubBy)
import Data.Maybe (Maybe, isJust)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = isJust $ findEntry firstName lastName book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy hasSameName book
  where
  hasSameName entry1 entry2 = 
    entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName
