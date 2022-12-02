{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Fixtures.Adhoc.HspecSpec (main, spec) where

import Control.Exception.Safe (bracket)
import Data.Fixtures.Adhoc
import Data.Fixtures.Adhoc.Hspec
import Data.IORef
import GHC.Exts (IsString (..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Adhoc fixtures" $
    aroundAll (withIORef @[(Key, Int)] mempty) $ do
      it "Tracker should be empty (start blank)" $ \tracker ->
        readIORef tracker `shouldReturn` []
      aroundAllWith (withFixtureAppendLift @"tracker" @_ @"box" boxFixture) $ do
        it "Tracker should have one key (added)" $ \fixture ->
          readIORef fixture.tracker `shouldReturn` [("box00", 42)]
        aroundAllWith (withFixtureAppend @"book" bookFixture) $ do
          it "Tracker should have two keys (added)" $ \fixture ->
            readIORef fixture.tracker `shouldReturn` [("book00", 42), ("box00", 42)]
        it "Tracker should have one key (cleaned)" $ \fixture ->
          readIORef fixture.tracker `shouldReturn` [("box00", 42)]
      it "Tracker should be empty (start cleaned)" $ \tracker ->
        readIORef tracker `shouldReturn` []

newtype Key = Key {getKey :: String}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

data Box = Box
  { boxId :: Int,
    boxKey :: Key
  }
  deriving stock (Eq, Show)

data Book = Book
  { bookId :: Int,
    bookBox :: Int,
    bookKey :: Key
  }
  deriving stock (Eq, Show)

type Tracker = IORef [(Key, Int)]

addId :: Key -> Int -> Tracker -> IO ()
addId k i t = modifyIORef' t ((k, i) :)

rmId :: Key -> Tracker -> IO ()
rmId k t = modifyIORef' t $ filter ((/= k) . fst)

unsafeGetId :: Key -> Tracker -> IO Int
unsafeGetId k t = snd . head . filter ((== k) . fst) <$> readIORef t

boxFixture ::
  HasFixture items "tracker" Tracker =>
  BuilderWith items IO "box" Box
boxFixture =
  buildWithClean
    (\prev -> let box = Box 42 "box00" in addId box.boxKey box.boxId prev.tracker >> return box)
    (\prev box -> rmId box.boxKey prev.tracker)

bookFixture ::
  (HasFixture items "tracker" Tracker, HasFixture items "box" Box) =>
  BuilderWith items IO "book" Book
bookFixture =
  buildWithClean
    ( \prev -> do
        box <- unsafeGetId "book00" prev.tracker
        let book = Book 42 box "book00"
        addId book.bookKey book.bookId prev.tracker
        return book
    )
    (\prev book -> rmId book.bookKey prev.tracker)

withIORef :: a -> (IORef a -> IO ()) -> IO ()
withIORef x =
  bracket
    (newIORef x)
    (const $ return ())
