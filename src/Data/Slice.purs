-- | A data type with O(1) take/drop using an array as the underlying storage.
module Data.Slice
  ( slice
  , Slice()
  , sarray
  , sat
  , scompare
  , seq
  , shead
  , slast
  , stail
  , sinit
  , snull
  , smap
  , sfind
  , sfindLast
  , sdrop
  , stake
  , szipWith
  , sfoldl
  , sfoldr
  , sempty
  , sappend
  , sconcat
  , sconcatMap
  ) where

import Prelude
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Data.Array (concat, length, (!!))
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (sequence, class Traversable)

newtype Slice a = Slice {base::Int, len::Int, arr::Array a}

slice :: forall a. Array a -> Slice a
slice a = Slice {base:0, len:length a, arr:a}

sstorage :: forall a. Slice a -> Array a
sstorage (Slice s) = s.arr

sarray :: forall a. Slice a -> Array a
sarray s = sstorage $ id <$> s

-- | Constructor for the empty slice.
sempty :: forall a. Slice a
sempty = Slice {base:0, len:0, arr:[]}

-- | Constructor for a slice containing a single element.
ssingleton:: forall a. a -> Slice a
ssingleton x = Slice {base:0, len:1, arr:[x]}

-- | Access an element at an index.
sat :: forall a. Slice a -> Int -> Maybe a
sat (Slice s) n = if n < s.len && n >= 0 then s.arr !! (s.base + n) else Nothing

-- | Equality test.
seq :: forall a. (Eq a) => Slice a -> Slice a -> Boolean
seq aa@(Slice a) bb@(Slice b) = a.len == b.len
                                && (foldl (&&) true $ szipWith (==) aa bb)

-- | Total order comparison.
scompare :: forall a. (Ord a) => Slice a -> Slice a -> Ordering
scompare a b = compare (sarray a) (sarray b)

imax :: Int -> Int -> Int
imax a b = if b < a then a else b

imin :: Int -> Int -> Int
imin a b = if a < b then a else b

-- | Drop a number of elements from the start of a slice, 
-- | creating a new slice (O(1)).
sdrop :: forall a. Int -> Slice a -> Slice a
sdrop n ss@(Slice s) = sfromLen cn cl ss
  where cn = imax 0 n
        nl = s.len - cn
        cl = imax 0 nl

-- | Keep only a number of elements from the start of a slice,
-- | creating a new slice (O(1)).
stake :: forall a. Int -> Slice a -> Slice a
stake n ss@(Slice s) = sfromLen 0 cl ss
  where cn = imax 0 n
        cl = imin s.len cn

sfromLen :: forall a. Int -> Int -> Slice a -> Slice a
sfromLen f l (Slice s) = Slice {base:s.base + f,
                                len:l,
                                arr:s.arr}


-- | Get the first element in a slice, or `Nothing` if the slice is empty.
shead :: forall a. Slice a -> Maybe a
shead s = s `sat` 0

-- | Get the last element in a slice, or `Nothing` if the slice is empty.
slast :: forall a. Slice a -> Maybe a
slast ss@(Slice s) = ss `sat` (s.len - 1)

-- | Get all but the last element of a slice, creating a new slice,
-- | or `Nothing` if the slice is empty (O(1)).
sinit :: forall a. Slice a -> Maybe (Slice a)
sinit s | snull s = Nothing
sinit ss@(Slice s) = Just $ stake (s.len - 1) ss

-- | Get all but the first element of a slice, creating a new slice,
-- | or `Nothing` if the slice is empty (O(1)).
stail :: forall a. Slice a -> Maybe (Slice a)
stail s | snull s = Nothing
stail s = Just $ sdrop 1 s

-- | Test whether a slice is empty.
snull :: forall a. Slice a -> Boolean
snull (Slice s) = s.len == 0

-- | Concatenate two slices, creating a new slice.
sappend :: forall a. Slice a -> Slice a -> Slice a
sappend a b = slice ((sarray a) <> (sarray b))

-- | Flatten a slice of slices, creating a new slice
sconcat :: forall a. Slice (Slice a) -> Slice a
sconcat xxs = slice $ concat $ sarray $ sarray <$> xxs

-- | Apply a function to each element in an array, and flatten the results
-- | into a single, new array.
sconcatMap :: forall a b. (a -> Slice b) -> Slice a -> Slice b
sconcatMap f xs = sconcat $ smap f xs


-- | Find the first index for which a predicate holds,
-- | or `-1` if no such element exists.
foreign import sfind :: forall a. (a -> Boolean) -> Slice a -> Int

-- | Find the last index for which a predicate holds,
-- | or `-1` if no such element exists.
foreign import sfindLast :: forall a. (a -> Boolean) -> Slice a -> Int

-- | Apply a function to each element in a slice, creating a new slice.
foreign import smap :: forall a b. (a -> b) -> Slice a -> Slice b

-- | Apply a left-folding function to a slice.
foreign import sfoldl :: forall a b. (b -> a -> b) -> b -> Slice a -> b

-- | Apply a right-folding function to a slice.
foreign import sfoldr :: forall a b. (a -> b -> b) -> b -> Slice a -> b

-- | Apply a function to pairs of elements at the same index in two slices,
-- | collecting the results in a new slice.
-- |
-- | If one slice is longer, elements will be discarded from the longer slice.
-- |
-- | For example
-- |
-- | ```purescript
-- | szipWith (*) (slice [1, 2, 3]) (slice [4, 5, 6, 7]) == slice [4, 10, 18]
-- | ```
foreign import szipWith :: forall a b c. (a -> b -> c) -> Slice a -> Slice b -> Slice c

instance showSlice :: (Show a) => Show (Slice a) where
  show s = "Slice [" <> intercalate ", " elems <> "]"
    where elems = smap show s

instance semigroupSlice :: Semigroup (Slice a) where
  append = sappend

instance monoidSlice :: Monoid (Slice a) where
  mempty = sempty

instance foldableSlice :: Foldable Slice where
  foldr = sfoldr
  foldl = sfoldl
  foldMap f xs = sfoldr (\x acc -> f x <> acc) mempty xs

instance traversableSlice :: Traversable Slice where
  traverse f = sequence <<< map f
  sequence xs = slice <$> (sequence $ sstorage xs)

instance eqSlice :: (Eq a) => Eq (Slice a) where
  eq = seq

instance ordSlice :: (Ord a) => Ord (Slice a) where
  compare = scompare

instance functorSlice :: Functor Slice where
  map = smap

instance applySlice :: Apply Slice where
  apply = ap

instance applicativeSlice :: Applicative Slice where
  pure = ssingleton

instance bindSlice :: Bind Slice where
  bind = flip sconcatMap

instance monadSlice :: Monad Slice

instance altSlice :: Alt Slice where
  alt = sappend
  
instance plusSlice :: Plus Slice where
  empty = sempty
  
instance alternativeSlice :: Alternative Slice

instance monadZeroSlice :: MonadZero Slice

instance monadPlusSlice :: MonadPlus Slice
