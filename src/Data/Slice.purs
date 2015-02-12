module Data.Slice
  ( slice
  , Slice()
  , sarray
  , sat
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


newtype Slice a = Slice {base::Number, len::Number, arr::[a]}

slice :: forall a. [a] -> Slice a
slice a = Slice {base:0, len:length a, arr:a}

sstorage :: forall a. Slice a -> [a]
sstorage (Slice s) = s.arr

sarray :: forall a. Slice a -> [a]
sarray s = sstorage $ id <$> s

sempty :: forall a. Slice a
sempty = slice []
  
ssingleton:: forall a. a -> Slice a
ssingleton x = slice [x]

sat :: forall a. Slice a -> Number -> Maybe a
sat (Slice s) n = if n < s.len && n >= 0 then s.arr !! (s.base + n) else Nothing

seq :: forall a. (Eq a) => Slice a -> Slice a -> Boolean
seq aa@(Slice a) bb@(Slice b) = a.len == b.len
                                && (foldl (&&) true $ szipWith (==) aa bb)

sdrop :: forall a. Number -> Slice a -> Slice a
sdrop n ss@(Slice s) = sfromLen cn cl ss
  where cn = max 0 n
        nl = s.len - cn
        cl = max 0 nl

stake :: forall a. Number -> Slice a -> Slice a
stake n ss@(Slice s) = sfromLen 0 cl ss
  where cn = max 0 n
        cl = min s.len cn

sfromLen :: forall a. Number -> Number -> Slice a -> Slice a
sfromLen f l (Slice s) = Slice {base:s.base + f,
                                len:l,
                                arr:s.arr}

shead :: forall a. Slice a -> Maybe a
shead s = s `sat` 0

slast :: forall a. Slice a -> Maybe a
slast ss@(Slice s) = ss `sat` (s.len - 1)

sinit :: forall a. Slice a -> Maybe (Slice a)
sinit s | snull s = Nothing
sinit ss@(Slice s) = Just $ stake (s.len - 1) ss

stail :: forall a. Slice a -> Maybe (Slice a)
stail s | snull s = Nothing
stail s = Just $ sdrop 1 s

snull :: forall a. Slice a -> Boolean
snull (Slice s) = s.len == 0


sappend :: forall a. Slice a -> Slice a -> Slice a
sappend a b = slice ((sarray a) <> (sarray b))

sconcat :: forall a. Slice (Slice a) -> Slice a
sconcat xxs = slice $ concat $ sarray $ sarray <$> xxs

sconcatMap :: forall a b. (a -> Slice b) -> Slice a -> Slice b
sconcatMap f xs = sconcat $ smap f xs


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


-- mapMaybe :: forall a b. (a -> Maybe b) -> Slice a -> Slice b
-- mapMaybe f = concatMap (maybe [] singleton <<< f)

-- catMaybes :: forall a. [Maybe a] -> [a]
-- catMaybes = concatMap (maybe [] singleton)

-- foreign import filter
--   "function filter (f) {\
--   \  return function (arr) {\
--   \    var n = 0;\
--   \    var result = [];\
--   \    for (var i = 0, l = arr.length; i < l; i++) {\
--   \      if (f(arr[i])) {\
--   \        result[n++] = arr[i];\
--   \      }\
--   \    }\
--   \    return result;\
--   \  };\
--   \}" :: forall a. (a -> Boolean) -> [a] -> [a]

-- foreign import range
--   "function range (start) {\
--   \  return function (end) {\
--   \    var i = ~~start, e = ~~end;\
--   \    var step = i > e ? -1 : 1;\
--   \    var result = [i], n = 1;\
--   \    while (i !== e) {\
--   \      i += step;\
--   \      result[n++] = i;\
--   \    }\
--   \    return result;\
--   \  };\
--   \}" :: Number -> Number -> [Number]

-- (..) :: Number -> Number -> [Number]
-- (..) = range

-- nub :: forall a. (Eq a) => [a] -> [a]
-- nub = nubBy (==)

-- nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
-- nubBy _ [] = []
-- nubBy (==) (x:xs) = x : nubBy (==) (filter (\y -> not (x == y)) xs)

-- sort :: forall a. (Ord a) => [a] -> [a]
-- sort xs = sortBy compare xs

-- sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
-- sortBy comp xs = sortJS comp' xs
--   where
--     comp' x y = case comp x y of
--       GT -> 1
--       EQ -> 0
--       LT -> -1

-- foreign import sortJS
--   "function sortJS (f) {\
--   \  return function (l) {\
--   \    return l.slice().sort(function (x, y) {\
--   \      return f(x)(y);\
--   \    });\
--   \  };\
--   \}" :: forall a. (a -> a -> Number) -> [a] -> [a]

-- group :: forall a. (Eq a) => [a] -> [[a]]
-- group xs = groupBy (==) xs

-- -- | Performs a sorting first.
-- group' :: forall a. (Ord a) => [a] -> [[a]]
-- group' = group <<< sort

-- groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
-- groupBy = go []
--   where
--   go :: forall a. [[a]] -> (a -> a -> Boolean) -> [a] -> [[a]]
--   go acc _  []     = reverse acc
--   go acc op (x:xs) = let sp = span (op x) xs in
--                      go ((x:sp.init):acc) op sp.rest

-- span :: forall a. (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
-- span = go []
--   where
--   go :: forall a. [a] -> (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
--   go acc p (x:xs) | p x = go (x:acc) p xs
--   go acc _ xs           = { init: reverse acc, rest: xs }

-- takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
-- takeWhile p xs = (span p xs).init

-- dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
-- dropWhile p xs = (span p xs).rest

foreign import ut
"""
function ut(a) {
  console.log(a);
  return a;
}
""" :: forall a. a -> a

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

