# Category-Syntax

(work in progress, not all features documented below are implemented)

Category-Syntax provides a Monad-like syntax for `Category` and various versions of "`Arrow` without `arr`". Once complete, it should looks like this:

```haskell
rearrange :: Either (Either a1 a2) (Either b1 b2)
          -> Either (Either a1 b1) (Either a2 b2)
rearrange = $(syntax [|do
    ((x1,x2), (y1,y2)) <- getInput
    returnC ((x1,y1), (x2,y2))
  |])
```

Which should expand to this:

```haskell
rearrange :: Either (Either a1 a2) (Either b1 b2)
          -> Either (Either a1 b1) (Either a2 b2)
            -- ((x1, x2), (y1,y2)))
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

One important feature of Category-Syntax is that it supports more than one set of rules. For example, if you write code which uses a variable more than once, then the variable will be duplicated via the `diag` method, which will impose a [`Contract`](https://github.com/gelisam/category-syntax/blob/master/src/Control/Category/Structural.hs) constraint on the generated code. If you try to use a variable more than once in a context which doesn't support the contraction rule, you will get a type error.

## Should I use Category-Syntax?

Most DSLs are monads. Use do-notation if you can.

If your language is not a `Monad`, then it's probably an `Applicative`. Use `<$>` and `<*>` if you can.

If your language is not a `Monad` nor an `Applicative`, then your language is probably a `Category`. Now we're talking! In general, if your language is implemented by a datatype which is more than a `Category` but less than an `Arrow`, then programs in your language will probably be more readable if they use Category-Syntax.

## Examples
### Bijections

Let's implement the famous ["seven trees in one"](http://blog.sigfpe.com/2007/09/arboreal-isomorphisms-from-nuclear.html) isomorphism. Dan Piponi's post on the subject is very readable, but since the isomorphism requires a large number of steps, the code he builds is unfortunately quite long and hard to read. With Category-Syntax, we can do much better.

We will need a `Category` for isomorphisms, and a data structure for trees:

```haskell
data Bij a b = Bij (a -> b) (b -> a)
inverse :: Bij a b -> Bij b a
instance Category Bij where ...

data Tree a = Leaf a | Branch (Tree a) (Tree a)
tree :: Bij (Tree a) (Either a (Tree a, Tree a))
```

In his blog post, Dan implements `commute`, `associate`, `associate'`, and `liftLeft`. These correspond to the structural operations `swap`, `associate`, `coassociate`, and `first`, which we need to implement ourselves because we are using a custom category.

```haskell
instance PFunctor Bij Either where first = ...
instance QFunctor Bij Either where second =...
instance Bifunctor Bij Either
instance Symmetric Bij Either where swap = ...
instance Associative Bij Either where ...
```

Now that we have implemented those structural rules, we can let Category-Syntax figure out when they need to be used. This greatly simplifies the code for describing the isomorphism, as we can now focus on the calls to `tree` and `inverse tree` instead of having to constantly rearrange the variables like in Dan's version.

For example, here is one especially hairy section from Dan's implementation.

```haskell
step15' :: T6 :+ T4 :+ T3 :+ T2 :+ T1 :+ T0 -> T7 :+ T5 :+ T4 :+ T3 :+ T2 :+ T1 :+ T0
step15' = liftLeft5 (commute . assemble')

shuffle5' :: T7 :+ T5 :+ T4 :+ T3 :+ T2 :+ T1 :+ T0 -> T7 :+ T5 :+ T3 :+ T2 :+ T1 :+ T0 :+ T4
shuffle5' = swap23 . liftLeft swap23 . liftLeft2 swap23 . liftLeft3 swap23

shuffle4' :: T7 :+ T5 :+ T3 :+ T2 :+ T1 :+ T0 :+ T4 -> T5 :+ T3 :+ T2 :+ T1 :+ T0 :+ T4 :+ T7
shuffle4' = swap23 . liftLeft swap23 . liftLeft2 swap23 . liftLeft3 swap23 . liftLeft4 swap23 . liftLeft5 commute

step14' :: T5 :+ T3 :+ T2 :+ T1 :+ T0 :+ T4 :+ T7 -> T4 :+ T2 :+ T1 :+ T0 :+ T4 :+ T7
step14' = liftLeft5 (assemble . commute)
```

And here is the equivalent part from our Category-Syntax version.

```haskell
    (t5', t7) <- tree t6'
    t4' <- inverse tree (t3, t5')
```

Even with such drastic simplifications, the full seven-trees-in-one isomorphism is still annoyingly long, but at least it's a lot easier to read now.

```haskell
iso :: Bij T1 T7
iso = $(syntax [|
    -- in this function, primed variables are consumed immediately
    t1' <- getInput
    
    (t0, t2') <- tree t1'
    (t1, t3') <- tree t2'
    (t2, t4') <- tree t3'
    (t3, t5') <- tree t4'
    (t4, t6') <- tree t5'
    (t5', t7) <- tree t6'
    
    -- still in scope: t0,t1,t2,t3,t4,  t7
    
    t4' <- inverse tree (t3, t5')
    t3' <- inverse tree (t2, t4')
    t2' <- inverse tree (t1, t3')
    t1  <- inverse tree (t0, t2')
    
    -- still in scope: t1,  t4,  t7
    
    (t6', t8) <- tree t7
    (t5', t7) <- tree t6'
    (t4', t6) <- tree t5'
    (t3', t5) <- tree t4'
    
    -- still in scope: t1,  t4,t5,t6,t7,t8
    
    t2' <- inverse tree (t1, t3')
    t3' <- inverse tree (t2', t4)
    t4' <- inverse tree (t3', t5)
    t5' <- inverse tree (t4', t6)
    t6' <- inverse tree (t5', t7)
    t7' <- inverse tree (t6', t8)
    
    returnC t7'
  |])
```

In order for the above to be a valid isomorphism, each variable must be used exactly once. Since `Bij` does not have a `Contract` instance, the fact that the above type-checks guarantees that variables match-up correctly.

### Wire Diagrams

Another great post from Dan Piponi is about [untangling knots](http://blog.sigfpe.com/2008/08/untangling-with-continued-fractions_16.html). He uses do-notation to represent a knot as a collection of curves and overlapping sections:

```haskell
example (a,b) = do
  (c,d) <- over (a,b)
  (e,f) <- cap
  (g,h) <- over (c,e)
  (i,j) <- over (f,d)
  (m,n) <- cap
  (k,l) <- cap
  (q,r) <- over (h,k)
  (s,y) <- over (l,i)
  (o,p) <- over (n,g)
  (t,u) <- under (p,q)
  (v,w) <- under (r,s)
  (x,z) <- over (y,j)
  cup (o,t)
  cup (u,v)
  cup (w,x)
  return (m,z)
```

We give names to the strands coming out of each section, and we use each name exactly once, as one of the two arguments of the next section in which the strand takes part.

With so many variables, it is easy for a typo to cause a variable to be used more than once, but with do-notation, the mistake will not be caught. Let's use Category-Syntax to create a safer knot-description language.

```haskell
data KnotSection a b where
  Over  :: KnotSection (a,b) (b,a)
  Under :: KnotSection (a,b) (b,a)
  Cap :: KnotSection () (a,a)
  Cup :: KnotSection (a,a) ()

type Knot = Linear KnotSection
over = linear Over
under = linear Under
cap = linear Cap
cup = linear Cup
```

The datatype `KnotSection` is not itself a `Category`, because it only represents the ways in which strands can interact, not the ways in which those interactions can be composed. To support those compositions, we use `Linear` to lift the base operations of `KnotSection` into a category with a particular structure. The structure for `Linear` is a "free symmetric monoidal category", that is, a structure in which variables can be freely paired and rearranged. Without the extra structure of a "cartesian category", which allows variables to be dropped and duplicated, the variables of a symmetric monoidal category must be used exactly once.

We can now describe our knot using almost the same code as before, except this time we can't accidentally drop or reuse a variable:

```haskell
example' :: Knot (a,b) (m,z)
example' = $(syntax [|do
    (a,b) <- getInput
    (c,d) <- over (a,b)
    (e,f) <- cap ()
    (g,h) <- over (c,e)
    (i,j) <- over (f,d)
    (m,n) <- cap ()
    (k,l) <- cap ()
    (q,r) <- over (h,k)
    (s,y) <- over (l,i)
    (o,p) <- over (n,g)
    (t,u) <- under (p,q)
    (v,w) <- under (r,s)
    (x,z) <- over (y,j)
    () <- cup (o,t)
    () <- cup (u,v)
    () <- cup (w,x)
    returnC (m,z)
  |])
```

If we make a mistake, we get a type error.

```haskell
-- No instance for (Contract Knot) arising from a use of ‘diag’
mistake :: Knot (a,b) (m,z)
mistake = $(syntax [|do
    (a,b) <- getInput
    () <- cup (a,b)
    returnC (a,b)
  |])
```

### Reference Counting

When a variable is dropped or reused, Category-Syntax inserts the corresponding structural operation between two user actions. By interpreting those structural operations accordingly, it should be possible to determine when a resource is last used, so it can be released as early as possible.

For simplicity, let's restrict our attention to a single variable. That is, if the variable isn't used at all, we want to free our resource immediately after the variable is bound, and if the variable is used as input to a few user actions, we want to free our resource immediately after the last of those actions.

For concreteness, here is a toy language in which there can only be one variable in scope, because every user action yields `()`.

```haskell
refCountExample :: RefCounted Int ()
refCountExample = $(syntax [|do
    x <- getInput
    () <- noop ()
    () <- useVar x
    () <- useVar x
    () <- noop ()
    () <- useVar x
    -- the resource should be released here
    () <- noop ()
    () <- noop ()
    returnC ()
  |])
```

Let's begin with the easy case: if a variable is never used, a call to `fst` or `snd` is inserted immediately after the variable is bound. That's our cue for releasing the resource:

```haskell
instance Weaken (,) RefCounted where
    fst = RefCounted $ \(x,y) -> do
        releaseResource y
        return x
    snd = RefCounted $ \(x,y) -> do
        releaseResource x
        return y
```

If a variable is used at least once, then a call to `diag` is inserted before all uses of the variable except for the last. This call creates a copy of the variable, so that's our cue for increasing its reference count. We assume that the internal representation of `RefCounted` has methods for incrementing and decrementing an associated counter, and that the counter is already initialized to 1.

```haskell
instance Contract (,) RefCounted where
    diag = RefCounted $ \x -> do
        increaseCounter x
        return (x,x)
```

Finally, the last time a variable is used, the variable gets consumed by the user action. There is no special structural operation for variable consumption, so we need every possible user action to decrement the counter and possibly release the resource. In the case of our toy language, there is only one:

```haskell
useVar :: RefCounted Int ()
useVar = RefCounted $ \x -> do
    r <- reallyUseVar x
    decreaseCounter x
    remaining <- readCounter x
    when (remaining == 0) $ do
      releaseResource x
    return r
```

Here is our example code again, with all the intermediate calls added by Category-Syntax.

```haskell
refCountExample :: RefCounted Int ()
                                    -- counter starts at 1
refCountExample = coidl             -- create ()
              >>> first noop
              >>> idl               -- drop ()
              >>> diag              -- increaseCounter, now 2
              >>> first (useVar x)  -- decreaseCounter, now 1
              >>> idl               -- drop ()
              >>> diag              -- increaseCounter, now 2
              >>> first (useVar x)  -- decreaseCounter, now 1
              >>> first noop
              >>> idl               -- drop ()
              >>> useVar x          -- decreaseCounter, now 0
                                    -- releaseResource
              >>> noop
              >>> noop
              >>> returnC
```

## Installation

To install the development version, clone this repository and use `cabal install` to compile Category-Syntax and its dependencies.
