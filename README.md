# Category Syntax

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

## Examples
### Seven Trees

Let's implement the famous ["seven trees in one"](http://blog.sigfpe.com/2007/09/arboreal-isomorphisms-from-nuclear.html) isomorphism. We will need a `Category` for isomorphisms, and a data structure for trees:

```haskell
data Bij a b = Bij (a -> b) (b -> a)
inverse :: Bij a b -> Bij b a
instance Category Bij where ...

data Tree a = Leaf a | Branch (Tree a) (Tree a)
tree :: Bij (Tree a) (Either a (Tree a, Tree a))
```

In his blog post, Sigfpe implements `commute`, `associate`, `associate'`, and `liftLeft`. These correspond to the structural operations `swap`, `associate`, coassociate`, and `first`, which we need to implement ourselves because we are using a custom category.

```haskell
instance PFunctor Bij Either where first = ...
instance QFunctor Bij Either where second =...
instance Bifunctor Bij Either
instance Symmetric Bij Either where swap = ...
instance Associative Bij Either where ...
```

Now that we have implemented those structural rules, we can let Category-Syntax figure out when they need to be used. This allows us to write a much more readable solution than in Sigfpe's otherwise great post.

```haskell
iso :: Bij T1 T7
iso = $(syntax [|
    t1                    <- getInput
    (t0, t2)              <- tree t1
    (  t1, t3)            <- tree t2
    (    t2, t4)          <- tree t3
    (      t3, t5)        <- tree t4
    (        t4, t6)      <- tree t5
    (          t5,  t7)   <- tree t6
    (            t6,  t8) <- tree t7
    (          t5', t7)   <- tree t6
    (        t4',t6)      <- tree t5'
    t3'                   <- inverse tree (    t2, t4')
    t2                    <- inverse tree (  t1, t3')
    t1                    <- inverse tree (t0, t2)
    t2                    <- inverse tree (  t1, t3)
    t3                    <- inverse tree (    t2, t4)
    t4                    <- inverse tree (      t3, t5)
    t5                    <- inverse tree (        t4, t6)
    t6                    <- inverse tree (          t5, t7)
    t7                    <- inverse tree (            t6, t8)
    returnC t7
  |]
```

In order for the above to be a valid isomorphism, each variable must be used exactly once. Since `Bij` does not have a `Contract` instance, the fact that the above type-checks guarantees that variables match-up correctly.
