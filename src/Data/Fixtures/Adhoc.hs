-- |
-- Module        : Data.Fixtures.Adhoc
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
-- > import Data.Fixtures.Adhoc
-- >
-- > type Person = Record '[Field "name" String, Field "age" Int]
-- >
-- > marvin :: Person
-- > marvin = Field "marvin" :> Field 42 :> RNil
-- >
-- > TODO
module Data.Fixtures.Adhoc
  ( Builder (..),
    buildWith,
    buildWithClean,
    runWithFixtures,
    -- runWithFixturesBracket,
    createFixtures,
  )
where

import Data.Kind
import Data.Records.Yarl.LinkedList
import GHC.Records
import GHC.TypeLits

data Builder m items = Builder
  { create :: m (Record items),
    clean :: Record items -> m ()
  }

buildWith :: a
buildWith = error "TODO"

buildWithClean :: a
buildWithClean = error "TODO"

runWithFixtures :: Monad m => Builder m items -> (Record items -> m a) -> m a
runWithFixtures builder act = do
  fixture <- builder . create
  result <- act fixture
  builder . clean fixture
  return result

createFixtures :: Monad m => Builder m items -> (Record items -> m a) -> m a
createFixtures builder act = builder . create >>= act
