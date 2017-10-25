{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Laws.Monoid (
  monoidLaws
  , semigroupLaws
  , leftIdentity
  , rightIdentity
  , monoidAssociative
  , semigroupAssociative
) where

import Data.Semigroup (Semigroup ((<>)))
import Data.Monoid (Monoid (mappend, mempty))
import Hedgehog

leftIdentity :: (Eq m, Show m, Monoid m) => Gen m -> Property
leftIdentity g = property $ do
  x <- forAll g
  mempty `mappend` x === x

rightIdentity :: (Eq m, Show m, Monoid m) => Gen m -> Property
rightIdentity g = property $ do
  x <- forAll g
  x `mappend` mempty === x

mkAssoc :: (Eq m, Show m) => (m -> m -> m) -> Gen m -> Property
mkAssoc (|+|) g = property $ do
  x <- forAll g
  y <- forAll g
  z <- forAll g
  x |+| (y |+| z) === (x |+| y) |+| z

monoidAssociative :: (Eq m, Show m, Monoid m) => Gen m -> Property
monoidAssociative = mkAssoc mappend

semigroupAssociative :: (Eq m, Show m, Semigroup m) => Gen m -> Property
semigroupAssociative = mkAssoc (<>)

monoidLaws :: (Eq m, Show m, Monoid m) => Gen m -> Group
monoidLaws g =
  Group "Monoid Laws"
    [ ("leftIdentity", leftIdentity g)
    , ("rightIdentity", rightIdentity g)
    , ("associativity", monoidAssociative g)
    ]

semigroupLaws :: (Eq m, Show m, Semigroup m) => Gen m -> Group
semigroupLaws g =
  Group "Semigroup Laws"
    [ ("associativity", semigroupAssociative g)
    ]

