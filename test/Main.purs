module Test.Main where

import Test.QuickCheck.Laws.Data.Functor
import Control.Alt (alt, class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (empty, class Plus)
import Data.Foldable (foldr, foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty, class Monoid)
import Data.Slice (scompare, Slice, szipWith, sfoldr, sfoldl, smap, sfindLast, sfind, snull, stail, sinit, slast, shead, stake, sdrop, sarray, sat, slice)
import Prelude (Ordering(..), append, class Semigroup, compare, class Ord, class Bind, class Monad, (<<<), class Applicative, eq, class Eq, class Apply, map, apply, class Functor, pure, Unit, bind, negate, not, show, (+), (==), ($), (<>), (*))
import Test.QuickCheck (QC, quickCheck')
import Test.QuickCheck.Arbitrary (arbitrary, class Arbitrary)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control (checkPlus, checkMonadZero, checkMonadPlus, checkMonad, checkBind, checkApply, checkApplicative, checkAlternative, checkAlt)
import Test.QuickCheck.Laws.Data (checkMonoid, checkSemigroup, checkOrd, checkEq)
import Type.Proxy (Proxy(Proxy), Proxy2(Proxy2))

newtype TestSlice a = TestSlice (Slice a)

unTestSlice :: forall a. TestSlice a -> Slice a
unTestSlice (TestSlice a) = a

instance arbTestSlice :: (Arbitrary a) => Arbitrary (TestSlice a) where
  arbitrary = do
    a <- arbitrary
    pure $ TestSlice $ slice a

instance eqTestSlice :: Eq a => Eq (TestSlice a) where
  eq (TestSlice a) (TestSlice b) = eq a b

instance ordTestSlice :: Ord a => Ord (TestSlice a) where
  compare (TestSlice a) (TestSlice b) = compare a b

instance functorTestSlice :: Functor TestSlice where
  map f (TestSlice xs) = TestSlice $ map f xs

instance applyTestSlice :: Apply TestSlice where
  apply (TestSlice fs) (TestSlice xs) = TestSlice $ apply fs xs

instance applicativeTestSlice :: Applicative TestSlice where
  pure = TestSlice <<< pure

instance bindTestSlice :: Bind TestSlice where
  bind (TestSlice xs) f = TestSlice $ bind xs $ unTestSlice <<< f

instance semigroupTestSlice :: Semigroup (TestSlice a) where
  append (TestSlice xs) (TestSlice ys) = TestSlice $ append xs ys

instance monoidTestSlice :: Monoid (TestSlice a) where
  mempty = TestSlice mempty

instance monadTestSlice :: Monad TestSlice

instance altTestSlice :: Alt TestSlice where
  alt (TestSlice xs) (TestSlice ys) = TestSlice $ alt xs ys

instance plusTestSlice :: Plus TestSlice where
  empty = TestSlice empty

instance alternativeTestSlice :: Alternative TestSlice

instance monadZeroTestSlice :: MonadZero TestSlice

instance monadPlusTestSlice :: MonadPlus TestSlice


main :: Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION) Unit
main = do
  let sz = sdrop 1 $ slice [0]
      s1 = sdrop 1 $ slice [0,1]
      s12 = sdrop 1 $ slice [0,1,2]      
      s23 = stake 2 $ sdrop 2 $ slice [0, 1, 2, 3, 4]
      s123 = stake 3 $ sdrop 1 $ slice [0, 1, 2, 3, 4]
      s234 = stake 3 $ sdrop 2 $ slice [0, 1, 2, 3, 4]      
      s246 = stake 3 $ sdrop 2 $ slice [0, 1, 2, 4, 6]
      s12321 = stake 5 $ sdrop 1 $ slice [0, 1, 2, 3, 2, 1, 4]      

  log "test sat"
  assert $ s123 `sat` 0 == (Just 1)
  assert $ s123 `sat` 1 == (Just 2)
  assert $ s123 `sat` 2 == (Just 3)
  assert $ s123 `sat` 3 == Nothing
  assert $ s123 `sat` (-1) == Nothing

  log "test show"
  assert $ show sz == "Slice []"
  assert $ show s1 == "Slice [1]"
  assert $ show s123 == "Slice [1, 2, 3]"

  log "test sarray"
  assert $ sarray s123 == [1, 2, 3]

  log "test sdrop"
  assert $ sdrop 1 s123 == s23
  assert $ sdrop 3 s123 == sz
  assert $ sdrop 4 s123 == sz
  assert $ sdrop (-1) s123 == s123

  log "test stake"
  assert $ stake 1 s123 == s1
  assert $ stake 3 s123 == s123
  assert $ stake 4 s123 == s123
  assert $ stake (-1) s123 == sz

  log "test shead"
  assert $ shead s123 == Just 1
  assert $ shead s1 == Just 1
  assert $ shead sz == Nothing

  log "test slast"
  assert $ slast s123 == Just 3
  assert $ slast s1 == Just 1
  assert $ slast sz == Nothing

  log "test sinit"
  assert $ sinit s123 == Just s12
  assert $ sinit s1 == Just sz
  assert $ sinit sz == Nothing

  log "test stail"
  assert $ stail s123 == Just s23
  assert $ stail s1 == Just sz
  assert $ stail sz == Nothing

  log "test snull"
  assert $ snull sz
  assert $ not $ snull s1

  log "test sfind"
  assert $ sfind ((==) 2) s12321 == 1
  assert $ sfind ((==) 0) s12321 == -1
  assert $ sfind ((==) 4) s12321 == -1

  log "test sfindLast"
  assert $ sfindLast ((==) 2) s12321 == 3
  assert $ sfindLast ((==) 0) s12321 == -1
  assert $ sfindLast ((==) 4) s12321 == -1

  log "test smap"
  assert $ smap (\x -> x * 2) s123 == s246

  log "test sfoldl"
  assert $ sfoldl (+) 0 s123 == 6
  assert $ sfoldl (\sofar x -> x) 0 s123 == 3
  assert $ sfoldl (+) 0 sz == 0

  log "test sfoldr"
  assert $ sfoldr (+) 0 s123 == 6
  assert $ sfoldr (\x sofar -> x) 0 s123 == 1
  assert $ sfoldr (+) 0 sz == 0

  log "test szipWith"
  assert $ szipWith (+) s123 s123 == s246
  assert $ szipWith (+) sz s123 == sz
  assert $ szipWith (+) s123 sz == sz
  assert $ szipWith (+) sz sz == sz     

  log "test <>"
  assert $ s1 <> s23 == s123

  log "test foldl"
  assert $ foldl (+) 0 s123 == 6
  assert $ foldl (\sofar x -> x) 0 s123 == 3
  assert $ foldl (+) 0 sz == 0


  log "test foldr"
  assert $ foldr (+) 0 s123 == 6
  assert $ foldr (\x sofar -> x) 0 s123 == 1
  assert $ foldr (+) 0 sz == 0

  log "test compare"
  assert $ scompare s123 s1 == GT
  assert $ scompare s1 s123 == LT
  assert $ scompare s1 s1 == EQ
  assert $ scompare sz sz == EQ
  assert $ scompare s123 s123 == EQ
  assert $ scompare sz s1 == LT

  checkLaws "Slice" $ do
    let p = Proxy :: Proxy (TestSlice A)
        p2 = Proxy2 :: Proxy2 TestSlice

    checkEq p
    checkOrd p
    checkFunctor p2
    checkAlt p2
    checkAlternative p2
    checkApplicative p2
    checkApply p2
    checkBind p2
    checkSemigroup p
    checkMonoid p
    checkMonad p2
    checkMonadPlus p2
    checkMonadZero p2
    checkPlus p2

  log "done"

assert :: Boolean -> QC () Unit
assert = quickCheck' 1


