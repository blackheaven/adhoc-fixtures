{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
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
    BuilderWith,
    HasFixture,
    buildWith,
    buildWithClean,
    build,
    buildClean,
    nullBuilder,
    pureBuilder,
    (&:),
    (&>),
    runWithFixtures,
    createFixtures,
  )
where

import Control.Exception.Safe (MonadMask, bracket)
import Data.Records.Yarl.LinkedList
import GHC.TypeLits

-- | Fixture builder (should be used directly with care)
data Builder m items = Builder
  { create :: m (Record items),
    clean :: Record items -> m ()
  }

-- | Builder relying on other builder(s)
type BuilderWith items m (name :: Symbol) a =
  HasNotField name items =>
  Builder m items -> Builder m (Field name a ': items)

-- | Helper around 'HasRecord'
type HasFixture items (name :: Symbol) a = HasField name (Record items) a

-- | Simple builder, no clean operation
buildWith ::
  forall (name :: Symbol) a m items.
  (Monad m, HasNotField name items) =>
  (Record items -> m a) ->
  BuilderWith items m name a
buildWith f = buildWithClean f $ \_ _ -> return ()

-- | Builder with cleaning operation
buildWithClean ::
  forall (name :: Symbol) a m items.
  (Monad m, HasNotField name items) =>
  (Record items -> m a) ->
  (Record items -> a -> m ()) ->
  BuilderWith items m name a
buildWithClean create' clean' previous =
  Builder
    { create = do
        xs <- previous.create
        x <- create' xs
        return $ Field x :> xs,
      clean =
        \(Field x :> xs) ->
          clean' xs x >> previous.clean xs
    }

-- | Simple builder without dependency, no clean operation
build ::
  forall (name :: Symbol) a m items.
  (Monad m, HasNotField name items) =>
  m a ->
  BuilderWith items m name a
build f = buildClean f $ const $ return ()

-- | Builder without dependency with cleaning operation
buildClean ::
  forall (name :: Symbol) a m items.
  (Monad m, HasNotField name items) =>
  m a ->
  (a -> m ()) ->
  BuilderWith items m name a
buildClean create' clean' previous =
  Builder
    { create = do
        xs <- previous.create
        x <- create'
        return $ Field x :> xs,
      clean =
        \(Field x :> xs) ->
          clean' x >> previous.clean xs
    }

-- | Base builder
nullBuilder :: Monad m => Builder m '[]
nullBuilder =
  Builder
    { create = return RNil,
      clean = const $ return ()
    }

-- | Pure builder
pureBuilder :: Monad m => Record items -> Builder m items
pureBuilder built =
  Builder
    { create = return built,
      clean = const $ return ()
    }

-- | Chain builders
(&:) :: HasNotField name items => BuilderWith items m name a -> Builder m items -> Builder m (Field name a ': items)
(&:) = ($)

infixr 5 &:

-- | Nest builders
(&>) :: (HasNotField name items, Monad m) => BuilderWith items m name a -> Record items -> Builder m (Field name a ': items)
(&>) builderWith built = builderWith &: pureBuilder built

infixr 5 &>

-- | Run fixtures with clean up (bracket)
runWithFixtures :: MonadMask m => Builder m items -> (Record items -> m a) -> m a
runWithFixtures builder = bracket builder.create builder.clean

-- | Create fixtures (no clean up)
createFixtures :: Monad m => Builder m items -> (Record items -> m a) -> m a
createFixtures builder act = builder.create >>= act
