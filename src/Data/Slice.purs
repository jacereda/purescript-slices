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

import Prelude
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.Monoid
import Data.Maybe
import Data.Array hiding(slice)
import Data.Foldable
--import Math

max :: Int -> Int -> Int
max a b = if a > b then a else b

min :: Int -> Int -> Int
min a b = if a < b then a else b


newtype Slice a = Slice {base::Int, len::Int, arr::Array a}

slice :: forall a. Array a -> Slice a
slice a = Slice {base:0, len:length a, arr:a}

sstorage :: forall a. Slice a -> Array a
sstorage (Slice s) = s.arr

sarray :: forall a. Slice a -> Array a
sarray s = sstorage $ id <$> s

sempty :: forall a. Slice a
sempty = slice []
  
ssingleton:: forall a. a -> Slice a
ssingleton x = slice [x]

sat :: forall a. Slice a -> Int -> Maybe a
sat (Slice s) n = if n < s.len && n >= 0 then s.arr !! (s.base + n) else Nothing

seq :: forall a. (Eq a) => Slice a -> Slice a -> Boolean
seq aa@(Slice a) bb@(Slice b) = a.len == b.len
                                && (foldl (&&) true $ szipWith (==) aa bb)

sdrop :: forall a. Int -> Slice a -> Slice a
sdrop n ss@(Slice s) = sfromLen cn cl ss
  where cn = max 0 n
        nl = s.len - cn
        cl = max 0 nl

stake :: forall a. Int -> Slice a -> Slice a
stake n ss@(Slice s) = sfromLen 0 cl ss
  where cn = max 0 n
        cl = min s.len cn

sfromLen :: forall a. Int -> Int -> Slice a -> Slice a
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


foreign import sfind :: forall a. (a -> Boolean) -> Slice a -> Int

foreign import sfindLast :: forall a. (a -> Boolean) -> Slice a -> Int

foreign import smap :: forall a b. (a -> b) -> Slice a -> Slice b

foreign import sfoldl :: forall a b. (b -> a -> b) -> b -> Slice a -> b

foreign import sfoldr :: forall a b. (a -> b -> b) -> b -> Slice a -> b


foreign import szipWith :: forall a b c. (a -> b -> c) -> Slice a -> Slice b -> Slice c

instance showSlice :: (Show a) => Show (Slice a) where
  show s = "Slice [" ++ intercalate ", " elems ++ "]"
    where elems = smap show s

instance semigroupSlice :: Semigroup (Slice a) where
  append = sappend

instance monoidSlice :: Monoid (Slice a) where
  mempty = slice []

instance foldableSlice :: Foldable Slice where
  foldr = sfoldr
  foldl = sfoldl
  foldMap f xs = sfoldr (\x acc -> f x <> acc) mempty xs

instance eqSlice :: (Eq a) => Eq (Slice a) where
  eq = seq
--  neq a b = not $ a == b

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

instance monadPlusSlice :: MonadPlus Slice

