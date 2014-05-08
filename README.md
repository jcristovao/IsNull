IsNull
======

A typeclass to determine if a given container is null.
Strongly inspired by 
[mono-traversable](https://hackage.haskell.org/package/mono-traversable), 
but with a simpler goal: supporting IsNull and nested IsNull operations.

Supported functions:

```
isNull :: a -> Bool

notNull :: a -> Bool

-- | Nested isNull
isNullN :: (Foldable f) => f a -> Bool

-- | Monadic isNull
isNullM :: Monad m => m a -> m Bool

-- | Monadic Nested isNull
isNullNM :: (Monad m, Foldable f) => m (f a) -> m Bool


-- | @'Alternative'@'s @'<|>'@ operator does not always operate as choice,
-- at least not in an intuitive way (for example with lists).
-- This one does:
<\> :: a -> a -> a
```

The N stands for (non-recursive) nested, such that:

```

isNullN (Just "abc") == False
isNullN (Just ""   ) == True
isNullN (Nothing   ) == True

```

While the ```isNull``` function is equivalent to ```(==) mempty``` 
for most of the instances,
not all ```Foldable```s are monoids: 
```Either``` for example. ```Either``` support was one
of the main reasons I defined this library.

Bugs, suggestions and comments are most welcome!
