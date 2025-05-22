# TODO

* In `DataFlow`:
  + `dirty` is currently a map, but could be a set, saving a little memory.
  + The documentation suggests that this algorithm allows data to flow
    along the edges of a *fixed* graph, but in fact, the algorithm should
    work also in the case where this is data dependency -- that is, the case
    where feeding more data (a higher property) into `foreach_successor`
    results into more data at each pre-existing successor
    *and possibly new successors*.
  + In the case where properties are sets and the transfer function
    is distributive (i.e., it is fully determined by its action at
    each singleton set), instead of marking a variable `x` as dirty
    and recomputing everything at `x`, we could keep track of a pair
    `(x, p)` where `p` is an increment (a partial property) and
    recompute/propagate just the consequences of `(x, p)`.

* The signature `NUMBERING` could be enriched (or a distinct
  signature could be defined) so as to be compatible with
  `CARDINAL` and so that `encode` and `decode` work with
  indices instead integers.

* Propose higher-level APIs (to be determined) for `Minimize`.

* Perform serious testing of `Minimize`.
  Try to find an online suite of (DFA, minimal DFA) pairs.

* Work on `Enum`. Decide whether it should be published.
  Decide whether `Partition` and `Minimize` should more heavily rely on it
  (e.g., by using `foreach` in every loop).

* In `Partition` and `Minimize`, instead of requiring an ordering on labels
  and using `Array.sort`, one could require the labels to be integers in a
  fixed range and use pigeonhole sort.

* In `MEMOIZER`, some variations are missing, e.g. `visibly_fix`,
  `visibly_defensive_fix`.

* Think about a heterogeneous version of the fixed point computation
  algorithm, where valuations have type `forall 'a. 'a variable -> 'a property`.
  (This would internally require using heterogenous maps...)

* Do something with `src/attic/BoolEqs` and `src/attic/ChopFix`,
  or remove them.

* Consider using two data fields in `node` instead of one,
  so as to avoid using a separate `data` record. Benchmark.

* Provide an extensible-vector implementation of `IMPERATIVE_MAPS` for
  integers? Like `ArraysAsImperativeMaps`, but does not require `n`.
      Use `InfiniteArray`.

* Provide an API in the style of Menhir's `FixSolver`, where constraints are
  discovered incrementally during a first phase, then the solver is started?

* Develop a test suite. (Use `afl-fuzz`?)
  E.g., in `CFG`, write a CFG generator.
  Compare `Fix` with a naive solver.

* Develop a performance benchmark.
