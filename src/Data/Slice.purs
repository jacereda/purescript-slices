-- | A data type with O(1) take/drop using an array as the underlying storage.
module Data.Slice
  ( slice
  , Slice()
  , sarray
  , sat
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

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.Monoid
import Data.Maybe
import Data.Array
import Data.Foldable
--import Math

max :: Number -> Number -> Number
max a b = if a > b then a else b

min :: Number -> Number -> Number
min a b = if a < b then a else b

-- | Represents a slice of an array.
newtype Slice a = Slice {base::Number, len::Number, arr::[a]}


-- | Create a slice from an array.
slice :: forall a. [a] -> Slice a
slice a = Slice {base:0, len:length a, arr:a}

-- | Access to the underlying storage. If the slice doesn't cover the whole array it will return the whole array anyways.
sstorage :: forall a. Slice a -> [a]
sstorage (Slice s) = s.arr

-- | Construct a new array from a slice.
sarray :: forall a. Slice a -> [a]
sarray s = sstorage $ id <$> s

-- | Constructor for the empty slice.
sempty :: forall a. Slice a
sempty = slice []

-- | Constructor for a slice containing a single element.
ssingleton:: forall a. a -> Slice a
ssingleton x = slice [x]

-- | Access an element at an index.
sat :: forall a. Slice a -> Number -> Maybe a
sat (Slice s) n = if n < s.len && n >= 0 then s.arr !! (s.base + n) else Nothing

-- | Equality test.
seq :: forall a. (Eq a) => Slice a -> Slice a -> Boolean
seq aa@(Slice a) bb@(Slice b) = a.len == b.len
                                && (foldl (&&) true $ szipWith (==) aa bb)

-- | Drop a number of elements from the start of a slice, 
-- | creating a new slice (O(1)).
sdrop :: forall a. Number -> Slice a -> Slice a
sdrop n ss@(Slice s) = sfromLen cn cl ss
  where cn = max 0 n
        nl = s.len - cn
        cl = max 0 nl

-- | Keep only a number of elements from the start of a slice,
-- | creating a new slice (O(1)).
stake :: forall a. Number -> Slice a -> Slice a
stake n ss@(Slice s) = sfromLen 0 cl ss
  where cn = max 0 n
        cl = min s.len cn

sfromLen :: forall a. Number -> Number -> Slice a -> Slice a
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
foreign import sfind
"""
function sfind(f) {
  return function(a) {
    for (var i = a.base>>>0, l = (a.base + a.len)>>>0; i < l; i++)
      if (f(a.arr[i]))
        return i - a.base;
    return -1;
  };
}
""" :: forall a. (a -> Boolean) -> Slice a -> Number

-- | Find the last index for which a predicate holds,
-- | or `-1` if no such element exists.
foreign import sfindLast
"""
function sfindLast(f) {
  return function(a) {
    for (var i = a.base + a.len - 1; i >= a.base; i--)
      if (f(a.arr[i]))
        return i - a.base;
    return -1;
  };
}
""" :: forall a. (a -> Boolean) -> Slice a -> Number

-- | Apply a function to each element in a slice, creating a new slice.
foreign import smap
"""
function smap (f) {
  return function (a) {
    var l = a.len >>> 0;
    var r = {base:0, len:l, arr:new Array(l)};
    for (var i = 0 >>> 0; i < l; i++)
      r.arr[i] = f(a.arr[a.base+i]);
    return r;
  };
}
""" :: forall a b. (a -> b) -> Slice a -> Slice b


-- | Apply a left-folding function to a slice.
foreign import sfoldl
"""
function sfoldl(f) {
  return function(z) {
    return function (a) {
      var r = z;
      var l = a.len >>> 0;
      var a1 = {base:0, len:l, arr:new Array(l)};
      for (var i = 0 >>> 0; i < l; i++)
        r = f(r)(a.arr[a.base+i]);
      return r;
    };
  };
}
""" :: forall a b. (b -> a -> b) -> b -> Slice a -> b

-- | Apply a right-folding function to a slice.
foreign import sfoldr
"""
function sfoldr(f) {
  return function(z) {
    return function (a) {
      var r = z;
      var l = a.len >>> 0;
      var a1 = {base:0, len:l, arr:new Array(l)};
      while (l--)
        r = f(a.arr[a.base+l])(r);
      return r;
    };
  };
}
""" :: forall a b. (a -> b -> b) -> b -> Slice a -> b


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
foreign import szipWith
"""
function szipWith(f) {
  return function(a) {
    return function(b) {
      var l = Math.min(a.len, b.len) >>> 0;
      var r = {base:0, len:l, arr:new Array(l)};
      for (var i = 0; i < l; i++)
        r.arr[i] = f(a.arr[a.base+i])(b.arr[b.base+i]);
      return r;
    };
  };
}
""" :: forall a b c. (a -> b -> c) -> Slice a -> Slice b -> Slice c

instance showSlice :: (Show a) => Show (Slice a) where
  show s = "Slice [" ++ intercalate ", " elems ++ "]"
    where elems = smap show s

instance semigroupSlice :: Semigroup (Slice a) where
  (<>) = sappend

instance monoidSlice :: Monoid (Slice a) where
  mempty = slice []

instance foldableSlice :: Foldable Slice where
  foldr = sfoldr
  foldl = sfoldl
  foldMap f xs = sfoldr (\x acc -> f x <> acc) mempty xs

instance eqSlice :: (Eq a) => Eq (Slice a) where
  (==) = seq
  (/=) a b = not $ a == b

instance functorSlice :: Functor Slice where
   (<$>) = smap

instance applySlice :: Apply Slice where
   (<*>) = ap

instance applicativeSlice :: Applicative Slice where
   pure = ssingleton

instance bindSlice :: Bind Slice where
   (>>=) = flip sconcatMap

instance monadSlice :: Monad Slice

instance altSlice :: Alt Slice where
   (<|>) = sappend
  
instance plusSlice :: Plus Slice where
   empty = sempty
  
instance alternativeSlice :: Alternative Slice

instance monadPlusSlice :: MonadPlus Slice

