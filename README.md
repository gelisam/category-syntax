Category Syntax
===

(work in progress)

Category-Syntax provides a Monad-like syntax for `Category` and various versions of "`Arrow` without `arr`". Once complete, it should looks like this:

```haskell
rearrange :: Either (Either a1 a2) (Either b1 b2)
          -> Either (Either a1 b1) (Either a2 b2)
rearrange = $(syntax [|do
    ((x1,x2), (y1,y2)) <- getInput
    returnC ((x1,y1), (x2,y2))
  |]
```

Which should expand to this:

```haskell
rearrange :: Either (Either a1 a2) (Either b1 b2)
          -> Either (Either a1 b1) (Either a2 b2)
rearrange = associate
            -- (x1, (x2, (y1,y2)))
        >>> second coassociate
            -- (x1, ((x2,y1), y2))
        >>> second (first swap)
            -- (x1, ((y1,x2), y2))
        >>> coassociate
            -- ((x1, (y1,x2)), y2)
        >>> first coassociate
            -- (((x1,y1), x2), y2)
        >>> associate
            -- ((x1,y1), (x2,y2))
        >>> returnC  -- aka Category.id
            -- ((x1,y1), (x2,y2))
```

In the above example, two structural rules have been used: associativity and commutativity. Some [substructural](http://en.wikipedia.org/wiki/Substructural_type_system) languages support more rules, some less.

One important feature of Category-Syntax is that is supports more than one set of rules. For example, if you write code which uses a variable more than once, then the variable will be duplicated via the `diag` method, which will impose a [`Contract`](https://github.com/gelisam/category-syntax/blob/master/src/Control/Category/Structural.hs) constraint on the generated code. If you try to use a variable more than once in a context which doesn't support the contraction rule, you will get a type error.
