IsNull
======

A typeclass to determine if a given container is null.
Strongly inspired by [mono-traversable](https://hackage.haskell.org/package/mono-traversable), 
but also supporting _Set_s. 

Supported functions:

```
isNull :: a -> Bool

notNull :: a -> Bool

isNullN :: (Foldable f) => f a -> Bool

isNullM :: Monad m => m a -> m Bool

isNullNM :: (Monad m, Foldable f) => m (f a) -> m Bool
```

The N stands for (non-recursive) nested, such as:

```

isNullN (Just "abc") == False
isNullN (Just ""   ) == True
isNullN (Nothing   ) == True

```

Bugs, suggestions and comments are most welcome!

_Note_: This dependens on a modified version of [base-compat](https://github.com/jcristovao/base-compat).
