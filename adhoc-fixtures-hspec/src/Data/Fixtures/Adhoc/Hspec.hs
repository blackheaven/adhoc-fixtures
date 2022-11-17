{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module        : Data.Fixtures.Adhoc.Hspec
-- Copyright     : Gautier DI FOLCO
-- License       : ISC
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : not portable
--
-- Fixtures builder and runner
--
-- Example:
--
-- > aroundAllWith (withFixtureAppendLift @"tracker" @_ @"box" boxFixture) $ do
-- >   it "Tracker should have one key (added)" $ \fixture ->
-- >     readIORef fixture.tracker `shouldReturn` [("box00", 42)]
module Data.Fixtures.Adhoc.Hspec
  ( withFixtureAppendLift,
    withFixtureAppend,
  )
where

import Data.Fixtures.Adhoc
import Data.Records.Yarl.LinkedList
import GHC.TypeLits
import Test.Hspec

-- | Fixture builder (should be used directly with care)
withFixtureAppendLift ::
  forall (name :: Symbol) a (builderName :: Symbol) b.
  HasNotField builderName '[Field name a] =>
  BuilderWith '[Field name a] IO builderName b ->
  ActionWith (Record '[Field builderName b, Field name a]) ->
  ActionWith a
withFixtureAppendLift builderWith runTest previousArgs =
  runWithFixtures (builderWith &: build @name (return previousArgs) &: nullBuilder) runTest

withFixtureAppend ::
  forall (builderName :: Symbol) b items.
  HasNotField builderName items =>
  BuilderWith items IO builderName b ->
  ActionWith (Record (Field builderName b ': items)) ->
  ActionWith (Record items)
withFixtureAppend builderWith runTest previousArgs =
  runWithFixtures (builderWith &> previousArgs) runTest
