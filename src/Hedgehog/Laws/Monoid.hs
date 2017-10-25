{-# LANGUAGE OverloadedStrings #-}

module Hedgehog.Laws.Monoid (
  monoidLaws
  , semigroupLaws
  , leftIdentity
  , rightIdentity
  , monoidAssociativity
  , semigroupAssociativity
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

monoidAssociativity :: (Eq m, Show m, Monoid m) => Gen m -> Property
monoidAssociativity = mkAssoc mappend

semigroupAssociativity :: (Eq m, Show m, Semigroup m) => Gen m -> Property
semigroupAssociativity = mkAssoc (<>)

monoidLaws :: (Eq m, Show m, Monoid m) => Gen m -> Group
monoidLaws g =
  Group "Monoid Laws"
    [ ("leftIdentity", leftIdentity g)
    , ("rightIdentity", rightIdentity g)
    , ("associativity", monoidAssociativity g)
    ]

semigroupLaws :: (Eq m, Show m, Semigroup m) => Gen m -> Group
semigroupLaws g =
  Group "Semigroup Laws"
    [ ("associativity", semigroupAssociativity g)
    ]

