# Module Documentation

## Module Data.Slice


A data type with O(1) take/drop using an array as the underlying storage.

#### `Slice`

``` purescript
newtype Slice a
```

Represents a slice of an array.

#### `slice`

``` purescript
slice :: forall a. [a] -> Slice a
```

Create a slice from an array.

#### `sarray`

``` purescript
sarray :: forall a. Slice a -> [a]
```

Construct a new array from a slice.

#### `sempty`

``` purescript
sempty :: forall a. Slice a
```

Constructor for the empty slice.

#### `sat`

``` purescript
sat :: forall a. Slice a -> Number -> Maybe a
```

Access an element at an index.

#### `seq`

``` purescript
seq :: forall a. (Eq a) => Slice a -> Slice a -> Boolean
```

Equality test.

#### `sdrop`

``` purescript
sdrop :: forall a. Number -> Slice a -> Slice a
```

Drop a number of elements from the start of a slice, 
creating a new slice (O(1)).

#### `stake`

``` purescript
stake :: forall a. Number -> Slice a -> Slice a
```

Keep only a number of elements from the start of a slice,
creating a new slice (O(1)).

#### `shead`

``` purescript
shead :: forall a. Slice a -> Maybe a
```

Get the first element in a slice, or `Nothing` if the slice is empty.

#### `slast`

``` purescript
slast :: forall a. Slice a -> Maybe a
```

Get the last element in a slice, or `Nothing` if the slice is empty.

#### `sinit`

``` purescript
sinit :: forall a. Slice a -> Maybe (Slice a)
```

Get all but the last element of a slice, creating a new slice,
or `Nothing` if the slice is empty (O(1)).

#### `stail`

``` purescript
stail :: forall a. Slice a -> Maybe (Slice a)
```

Get all but the first element of a slice, creating a new slice,
or `Nothing` if the slice is empty (O(1)).

#### `snull`

``` purescript
snull :: forall a. Slice a -> Boolean
```

Test whether a slice is empty.

#### `sappend`

``` purescript
sappend :: forall a. Slice a -> Slice a -> Slice a
```

Concatenate two slices, creating a new slice.

#### `sconcat`

``` purescript
sconcat :: forall a. Slice (Slice a) -> Slice a
```

Flatten a slice of slices, creating a new slice

#### `sconcatMap`

``` purescript
sconcatMap :: forall a b. (a -> Slice b) -> Slice a -> Slice b
```

Apply a function to each element in an array, and flatten the results
into a single, new array.

#### `sfind`

``` purescript
sfind :: forall a. (a -> Boolean) -> Slice a -> Number
```

Find the first index for which a predicate holds,
or `-1` if no such element exists.

#### `sfindLast`

``` purescript
sfindLast :: forall a. (a -> Boolean) -> Slice a -> Number
```

Find the last index for which a predicate holds,
or `-1` if no such element exists.

#### `smap`

``` purescript
smap :: forall a b. (a -> b) -> Slice a -> Slice b
```

Apply a function to each element in a slice, creating a new slice.

#### `sfoldl`

``` purescript
sfoldl :: forall a b. (b -> a -> b) -> b -> Slice a -> b
```

Apply a left-folding function to a slice.

#### `sfoldr`

``` purescript
sfoldr :: forall a b. (a -> b -> b) -> b -> Slice a -> b
```

Apply a right-folding function to a slice.

#### `szipWith`

``` purescript
szipWith :: forall a b c. (a -> b -> c) -> Slice a -> Slice b -> Slice c
```

Apply a function to pairs of elements at the same index in two slices,
collecting the results in a new slice.

If one slice is longer, elements will be discarded from the longer slice.

For example

```purescript
szipWith (*) (slice [1, 2, 3]) (slice [4, 5, 6, 7]) == slice [4, 10, 18]
```

#### `showSlice`

``` purescript
instance showSlice :: (Show a) => Show (Slice a)
```


#### `semigroupSlice`

``` purescript
instance semigroupSlice :: Semigroup (Slice a)
```


#### `monoidSlice`

``` purescript
instance monoidSlice :: Monoid (Slice a)
```


#### `foldableSlice`

``` purescript
instance foldableSlice :: Foldable Slice
```


#### `eqSlice`

``` purescript
instance eqSlice :: (Eq a) => Eq (Slice a)
```


#### `functorSlice`

``` purescript
instance functorSlice :: Functor Slice
```


#### `applySlice`

``` purescript
instance applySlice :: Apply Slice
```


#### `applicativeSlice`

``` purescript
instance applicativeSlice :: Applicative Slice
```


#### `bindSlice`

``` purescript
instance bindSlice :: Bind Slice
```


#### `monadSlice`

``` purescript
instance monadSlice :: Monad Slice
```


#### `altSlice`

``` purescript
instance altSlice :: Alt Slice
```


#### `plusSlice`

``` purescript
instance plusSlice :: Plus Slice
```


#### `alternativeSlice`

``` purescript
instance alternativeSlice :: Alternative Slice
```


#### `monadPlusSlice`

``` purescript
instance monadPlusSlice :: MonadPlus Slice
```




