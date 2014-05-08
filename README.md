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


-- | Alternative <|> operator does not always operate as choice,
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
for most of the instances, not all ```Foldable```s are monoids,
and not all monoids ```mempty``` means null:

* ```Either``` is an example of a ```Foldable``` which is not a
  ```Monoid```, but where it makes sense to consider a ```Left``` as
  an 'Null' value. While this is not strictly true, the ```Left```
  option does carries a value, we take the more liberal approach:
  Empty ~ Null ~ Invalid Value.
  If you need proper type reasoning, you should not be using this
  package, just regular pattern matching instead.

* ```Product``` ```Monoid``` instance is ```1```. Hardly qualifies as an
  ```Empty``` or ```Null``` value. For this reason no default implementation
  is provided for the ```Monoid``` class. It's up to you to use
  ```(==) mempty``` instead.


Bugs, suggestions and comments are most welcomed!
