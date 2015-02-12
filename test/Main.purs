module Test.Main where

import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Classes
import Data.Maybe
import Data.Slice
import Data.Foldable

instance arbSlice :: (Arbitrary a) => Arbitrary (Slice a) where
  arbitrary = do
    b <- arbitrary
    if b then return sempty else do
      a <- arbitrary
      as <- arbitrary
      return $ a <> as
          
main = do
  let sz = sdrop 1 $ slice [0]
      s1 = sdrop 1 $ slice [0,1]
      s12 = sdrop 1 $ slice [0,1,2]      
      s23 = stake 2 $ sdrop 2 $ slice [0, 1, 2, 3, 4]
      s123 = stake 3 $ sdrop 1 $ slice [0, 1, 2, 3, 4]
      s234 = stake 3 $ sdrop 2 $ slice [0, 1, 2, 3, 4]      
      s246 = stake 3 $ sdrop 2 $ slice [0, 1, 2, 4, 6]
      s12321 = stake 5 $ sdrop 1 $ slice [0, 1, 2, 3, 2, 1, 4]      


  trace "test sat"
  assert $ s123 `sat` 0 == (Just 1)
  assert $ s123 `sat` 1 == (Just 2)
  assert $ s123 `sat` 2 == (Just 3)
  assert $ s123 `sat` 3 == Nothing
  assert $ s123 `sat` (-1) == Nothing

  trace "test show"
  assert $ show sz == "Slice []"
  assert $ show s1 == "Slice [1]"
  assert $ show s123 == "Slice [1, 2, 3]"

  trace "test sarray"
  assert $ sarray s123 == [1, 2, 3]

  trace "test sdrop"
  assert $ sdrop 1 s123 == s23
  assert $ sdrop 3 s123 == sz
  assert $ sdrop 4 s123 == sz
  assert $ sdrop (-1) s123 == s123

  trace "test stake"
  assert $ stake 1 s123 == s1
  assert $ stake 3 s123 == s123
  assert $ stake 4 s123 == s123
  assert $ stake (-1) s123 == sz

  trace "test shead"
  assert $ shead s123 == Just 1
  assert $ shead s1 == Just 1
  assert $ shead sz == Nothing

  trace "test slast"
  assert $ slast s123 == Just 3
  assert $ slast s1 == Just 1
  assert $ slast sz == Nothing

  trace "test sinit"
  assert $ sinit s123 == Just s12
  assert $ sinit s1 == Just sz
  assert $ sinit sz == Nothing

  trace "test stail"
  assert $ stail s123 == Just s23
  assert $ stail s1 == Just sz
  assert $ stail sz == Nothing

  trace "test snull"
  assert $ snull sz
  assert $ not $ snull s1

  trace "test sfind"
  assert $ sfind ((==) 2) s12321 == 1
  assert $ sfind ((==) 0) s12321 == -1
  assert $ sfind ((==) 4) s12321 == -1

  trace "test sfindLast"
  assert $ sfindLast ((==) 2) s12321 == 3
  assert $ sfindLast ((==) 0) s12321 == -1
  assert $ sfindLast ((==) 4) s12321 == -1

  trace "test smap"
  assert $ smap (\x -> x * 2) s123 == s246

  trace "test sfoldl"
  assert $ sfoldl (+) 0 s123 == 6
  assert $ sfoldl (\sofar x -> x) 0 s123 == 3
  assert $ sfoldl (+) 0 sz == 0

  trace "test sfoldr"
  assert $ sfoldr (+) 0 s123 == 6
  assert $ sfoldr (\x sofar -> x) 0 s123 == 1
  assert $ sfoldr (+) 0 sz == 0

  trace "test szipWith"
  assert $ szipWith (+) s123 s123 == s246
  assert $ szipWith (+) sz s123 == sz
  assert $ szipWith (+) s123 sz == sz
  assert $ szipWith (+) sz sz == sz     

  trace "test <>"
  assert $ s1 <> s23 == s123

  trace "test foldl"
  assert $ foldl (+) 0 s123 == 6
  assert $ foldl (\sofar x -> x) 0 s123 == 3
  assert $ foldl (+) 0 sz == 0


  trace "test foldr"
  assert $ foldr (+) 0 s123 == 6
  assert $ foldr (\x sofar -> x) 0 s123 == 1
  assert $ foldr (+) 0 sz == 0

  trace "test functor laws"
  checkFunctor s1

  trace "test applicative laws"
  checkApplicative s1 s1 s1

  trace "test monad laws"
  checkMonad s1
  

  return unit

assert :: Boolean -> QC Unit
assert = quickCheck' 1


