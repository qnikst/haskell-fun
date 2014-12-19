Region-based resource management
================================

For more information see [Oleg's site](http://okmij.org/ftp/Haskell/regions.html)

Here I'm trying to solve some regions related questions
and experiment.

All information below was archived based on discussion with Oleg Kiselov,
however I could missinterpret some parts.

TODO:
-----

1. Get rid of overlapping instances.

> We can do that if we had equality
> predicate, like TEQ in the commented-out section of
> SafeHandles. Previously, TEQ needed overlapping instances. However, it
> can be implemented with closed type families. Therefore, overlapping
> instances are no longer required. I

While trying to solve TEQ I eventually hit into a possible ghc bug.
see SafeHandlesTEQ.hs

2. try to have SMonadIO = RMonadIO

> It might be interesting to see if SMonadIO can be defined simply as an
> alias
>        type SMonadIO = RMonadIO
> It could work if GHC allows exporting SMonadIO but not RMonadIO. This
> requires some experimentation; I'm not sure of exact scoping/alias
> resolution rules.


1. Using resourceForkIO
------------------------

The first problem is how to use monadic regions in presence of concurrency.
The idea is to use `resouceForIO` method that should start a new thread
with some sane region properties.


Respecting properties of monadic regions tha is 

> any value that is visible and allowed to be used by typechecker is safe

leads us to a choice:

1. we can start new toplevel
2. we can start region with current region as a parent

Former approach is quite restrictive as there will be no way to pass values
to the new region. And as soon will be shown it's possible to express first
using second approach. As we can lift our regionForkIO to upper region we
can select the region that will be shared (up to toplevel one). Then
all resources that exist in shared region and upper will be protected and
available in both regions for their livetime.

As a result we are building a semi-lattice (i.e. a region tree, where branches
are forks).


Other ideas:
------------

1. Region tagged variables, it would be nice to tag some pure values that
can delay execution can we unsafe to use outside a region. 
From one point of view there should be no such values, but if they stil exists?

2. Region variables. Another interesting approach is to use variables that
create region context on their own. This will allow to pass resources between
regions in a safe way (but with unsafe internals).

    data RContext = ..
    putRContext :: RContext -> handle s -> R s m ()
    useRContext :: RConctext -> (forall q {-such s is q`s parent-} . handle q -> R q a) -> R s a

  so region variable is capable for a livetime of values that are inside it.
  This approach have downsides, for example if resource allocation locks some
  external resource then we can't reason about the time when resource is free
  to allocate again. (Same true for regionForkIO).


