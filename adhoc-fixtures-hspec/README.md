# adhoc-fixtures

Helps improves tests crafting per-test fixtures.

## Example

```haskell
aroundAllWith (withFixtureAppendLift @"tracker" @_ @"box" boxFixture) $ do
  it "Tracker should have one key (added)" $ \fixture ->
      readIORef fixture.tracker `shouldReturn` [("box00", 42)]

-- ...
boxFixture ::
  HasFixture items "tracker" Tracker =>
  BuilderWith items IO "box" Box
boxFixture =
  buildWithClean
  (\prev -> let box = Box 42 "box00" in addId box.boxKey box.boxId prev.tracker >> return box)
  (\prev box -> rmId box.boxKey prev.tracker)
```

