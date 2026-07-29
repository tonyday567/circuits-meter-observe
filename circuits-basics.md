# circuits basics — timing and space for circuits components

Benchmarks of `circuits` library components. All measurements: p50 quantile,
50 runs unless noted, Apple M4 Pro, GHC 9.14.1, `circuits-meter` bracket
(42 ns overhead).

## tree traversal — stack thunks vs heap continuation

Three ways to sum a binary tree. "Length" is tree depth; a left-spine tree
of depth n has 2n+1 nodes. Direct recursion builds O(depth) thunks on the
GHC eval stack; the explicit-stack and Either-trace versions put the
continuation on the heap.

### variants

```haskell
-- Direct non-tail recursion — builds thunks
sumTreeDirect (Leaf n)     = n
sumTreeDirect (Node l r)   = sumTreeDirect l + sumTreeDirect r

-- Explicit heap stack — tail-recursive, O(depth) heap
sumTreeStack t = go t [] 0
  where
    go (Leaf n) stack acc   = case stack of
      [] -> acc + n; (t':s') -> go t' s' (acc + n)
    go (Node l r) stack acc = go l (r : stack) acc

-- Either trace — delimited continuation via Traced Either (->)
sumTreeEither t = trace (either step step) (t, [], 0)
  where
    step (Leaf n, stack, acc)   = case stack of
      [] -> Right (acc + n); (t':s') -> Left (t', s', acc + n)
    step (Node l r, stack, acc) = Left (l, r : stack, acc)
```

### scaling data

| depth | sumTreeDirect | sumTreeStack | sumTreeEither |
|-------|--------------|-------------|---------------|
| 100   | 7.5 ns/elem  | 7.9         | 15.0          |
| 200   | 7.7          | 8.3         | 15.6          |
| 500   | 7.5          | 7.7         | 14.4          |
| 1000  | 7.7          | 7.4         | 15.3          |
| 2000  | 7.2          | 7.5         | 19.8          |
| 5000  | 10.4         | 7.6         | 20.1          |
| 10000 | 11.5         | 7.7         | 21.5          |
| 20000 | 11.4         | 7.8         | —             |

All variants p50, 50 runs (30 runs at depth 5000+, 20 at 20000).
ns/elem = total time ÷ depth (not node count — multiply by ~2 for per-node cost).

### findings

**Explicit heap stack wins.** `sumTreeStack` is constant ~7.6 ns/elem
regardless of depth, 2% spread. The continuation (pending right subtrees)
lives on the heap as a `[Tree]` list. Tail-recursive loop, no thunk buildup,
no GC pressure. This is the pattern to beat.

**Direct recursion degrades.** `sumTreeDirect` starts at 7.5 ns/elem but
climbs to 11.4 at depth 20000. The `l + (r + ...)` thunk chain is O(depth)
and GC pressure grows with depth. The spread widens from 7% to 11% as the
thunk becomes a GC target. At depth 5000 the crossover is visible; by 20000
the direct version is 1.5× the stack version and still climbing.

**Either trace carries machinery overhead.** `sumTreeEither` is 2.0–2.7×
slower than the explicit stack at all depths. The ~13 ns/elem overhead breaks
down as:

- `either step step` — two case branches on `Either`, both doing the same work
- `(Tree, [Tree], Int)` tuple allocation per iteration
- `trace` → `go (Right state)` / `Left → go (Left state)` wrapper

The `Traced Either (->)` Core shows a `letrec` `go_r1kD` with boxed
accumulator (`$fNumInt_$c+`) and state-tuple pattern matching on every
iteration. The continuation pattern is structurally identical to
`sumTreeStack` but the encoding through the `Traced` class adds constant
overhead.

**Why Either trace exists.** The machinery overhead (~13 ns) is the cost of
a lawful, categorical interface. `Traced Either arr` works for any `arr`
with the right structure — `(->)`, `Kleisli m`, `Hyper arr`. The explicit
stack version only works in `(->)`. When you need the same loop in a monadic
or traced setting, the `Traced` class earns its keep. The overhead is
measurable and bounded — 13 ns is the price of abstraction.

### Core comparison

`sumTreeDirect` gets a worker/wrapper with unboxed `Int#` but is non-tail
(value recursion):

```haskell
$wsumTreeDirect :: Tree -> Int#
$wsumTreeDirect = \ ds -> case ds of
  Leaf n    -> case n of I# y -> y
  Node l r  -> case $wsumTreeDirect l of ww ->
               case $wsumTreeDirect r of ww1 -> +# ww ww1
```

`sumTreeEither` compiles to a `letrec` go-loop with boxed accumulator and
state-tuple destructuring per iteration:

```haskell
go_r1kD :: Either (Tree, [Tree], Int) (Tree, [Tree], Int) -> Int
go_r1kD = \ x -> case x of
  Left (ds, acc) -> ...
    Node l r -> go_r1kD (Left (l, r : stack, acc))
  Right (ds, acc) -> ... same ...
```

The extra `Left`/`Right` case + tuple pattern match per iteration is the
13 ns overhead vs `sumTreeStack`'s direct tail call.

## Hyper — cost of the hyperfunction encoding

Timing the core `Hyper` (= `HyperF (->)`) operations: lift, observe, push,
compose, nest, and runHyper. Each functions' "length" parameter controls
the number of times the operation is applied.

### variants

```haskell
-- lift + observe round-trip, called N times
hyperRoundTrip n = go n 0
  where go 0 acc = acc; go i acc = go (i-1) (observe (lift (+1)) acc)

-- push N functions onto a base, then observe once
hyperPushChain n = observe (go n (base 0)) n
  where go 0 h = push (+1) h; go i h = go (i-1) (push (+1) h)

-- compose N hyperfunctions with Category (.)
hyperCompose n = observe (go n) 0
  where go 0 = lift (+1); go i = go (i-1) . lift (+1)

-- invoke-chain traversal — feed one Hyper as continuation to the next
hyperNest n = go n (lift (+1))
  where go 0 h = observe h 0; go i h = go (i-1) (Hyper $ \k -> invoke h k)

-- runHyper on a self-referential loop, N times
hyperRun n = go n 0
  where go 0 acc = acc
        go i acc = go (i-1) (runHyper (Hyper $ \k -> invoke k (Hyper (const (acc+1)))))

-- direct function application baseline
directCompose n = go n 0
  where go 0 x = x+1; go i x = go (i-1) (x+1)
```

### scaling data

| length | hyperRoundTrip | hyperPushChain | hyperCompose | hyperNest | hyperRun | directCompose |
|--------|---------------|----------------|-------------|-----------|----------|---------------|
| 100    | 22.1 ns/elem  | 1.25           | 17.5        | 1.66      | 10.8     | 1.25          |
| 500    | 20.6          | 0.92           | 16.5        | 0.83      | 10.8     | 0.75          |
| 1000   | 20.2          | 0.79           | 16.0        | 0.71      | 10.3     | 0.71          |
| 5000   | 32.4          | 0.72           | 21.4        | 0.67      | 15.2     | 0.66          |

All p50, 50 runs. ns/elem = total time ÷ length.

### findings

**push is cheap.** At ~0.7–1.3 ns, pushing a function onto a Hyper is
comparable to direct function application. The `pushH` implementation is
thin: `mkArr $ \k -> runArr (invoke k) h >> runArr f a_val`. One
continuation call + one function application.

**compose is expensive.** At 16–21 ns, each `(.)` on `HyperF arr` creates
a new continuation wrapper:

```haskell
f . g = HyperF $ mkArr $ \k -> runArr (invoke f) (g . k)
```

The `g . k` builds a closure chain. Each composition adds one layer of
indirection through the continuation. 16–30× the cost of `push`.

**nest is cheap.** Direct `Hyper` constructor + `invoke` traversal at
~0.7–1.7 ns/step. Each step is `Hyper $ \k -> invoke h k` — wrap a
Hyper in one more layer of continuation. The encoding overhead is a
single closure allocation + field access.

**runHyper costs ~10–15 ns/knot.** Each call creates a `Hyper` closure,
calls `runHyper` (which uses `fixRun` — lazy knot tying via `let x = f x`),
and extracts the result. About 15× the cost of a direct function call.

**roundTrip highlights lift's coinductive nature.** `lift f = pushH f (lift f)`
builds a self-referential Hyper. Each `lift` call pushes `f` onto the
infinite chain. `observe` extracts it back. At 20–33 ns, the cost is
dominated by the coinductive `lift` construction — `observe` alone is
just field access (`invoke`).

### cost model

| operation | ns/call | relative to direct |
|-----------|---------|-------------------|
| direct call | 0.7 | 1× |
| push | 1.0 | 1.4× |
| nest (invoke chain) | 0.8 | 1.1× |
| runHyper (knot) | 12 | 17× |
| compose (.) | 18 | 26× |
| lift + observe | 25 | 36× |

For performance-sensitive Hyper usage: prefer `push` over `(.)`. Build
chains via `push` rather than composition when possible. The `lift` +
`observe` round-trip is the most expensive common operation — avoid
repeated lift/observe pairs in hot loops.

## Traced (,) — lazy knot

The cartesian trace (`Traced (,) (->)`) ties a lazy knot:
`trace f b = let ~(a, c) = f (a, b) in c`. Only works for productive
(early-output) functions — the feedback value `a` appears on both sides
of the equation and must be producible without forcing itself.

### variants

```haskell
-- Trace: uses the class method
traceLazyKnot n = head $ drop n (trace step ())
  where step (ns, ()) = (0 : map (+1) ns, ns)

-- Direct: same knot without the trace wrapper
directKnot n = head $ drop n ns
  where ~(ns, _) = (0 : map (+1) ns, ())
```

### scaling data

| length | traceLazyKnot | directKnot |
|--------|--------------|------------|
| 100    | 3.3 ns       | 3.3        |
| 500    | 3.1          | 3.0        |
| 1000   | 3.0          | 3.0        |
| 5000   | 3.4          | 3.3        |

All p50, 50 runs. ns/elem = total ÷ n (index into lazy list).

### findings

**Trace overhead is negligible.** At ~3.2 ns/elem, `trace` is identical to
the direct lazy knot. The `Traced` class dictionary dispatch adds no
measurable overhead — the Core for both is the same lazy pattern match.

**Lazy knots are cheap.** 3.2 ns/elem is about 5× the cost of a direct
function call (0.7 ns). The overhead is the lazy pattern match + cons
allocation per element traversed. Not the knot tying itself — that
happens once.

## Loop — free traced monoidal category

### variants

```haskell
-- Lift a function into Loop and run it back
loopLift n = go n 0
  where go 0 acc = acc; go i acc = go (i-1) (run (Lift (+1) :: Loop (,) (->) Int Int) acc)

-- Knot a feedback loop (lazy list) and run it
loopKnot n = head $ drop n (run knot ())
  where step (ns, ()) = (0 : map (+1) ns, ns)
        knot = Knot step :: Loop (,) (->) () [Int]

-- Compose N Lifts and run
loopCompose n = run (go n :: Loop (,) (->) Int Int) 0
  where go 0 = Lift (+1); go i = go (i-1) . Lift (+1)
```

### scaling data

| length | loopLift | loopKnot | loopCompose |
|--------|---------|----------|-------------|
| 100    | 1.3 ns  | 3.3      | 30.4        |
| 500    | 0.8     | 3.1      | 30.0        |
| 1000   | 0.8     | 3.1      | 30.3        |
| 5000   | 0.7     | 2.2      | 39.6        |

All p50, 50 runs. ns/elem = total ÷ n.

### findings

**Lift+run is nearly free.** At ~0.9 ns, `run (Lift f)` is barely above
direct function application. The `Lift` constructor is a newtype wrapper
that GHC eliminates; `run` just strips it. Spread is high (83%) because
the measurement is at the noise floor.

**Knot+run costs ~3 ns.** The overhead over Lift is the lazy knot tying +
list cons per element. Same ballpark as `traceLazyKnot`/`directKnot`.

**Loop composition is expensive.** At ~33 ns per composition, each `(.)`
on Loop creates a new existential GADT node. The `Loop` Category instance
is: `Lift f . Lift g = Lift (f . g)` — but for non-trivial chains the
GADT matching and reconstruction adds up. 30-40× the cost of direct
function composition.

## Net — free traced PROP

### variant

```haskell
netRoundTrip n = go n 0
  where go i acc = go (i-1) (run (melt (enrich (Lift (+1) :: Loop (,) (->) Int Int)) :: Loop (,) (->) Int Int) acc)
```

### scaling data

| length | netRoundTrip |
|--------|-------------|
| 100    | 10.0 ns     |
| 500    | 10.0        |
| 1000   | 7.6         |
| 5000   | 12.7        |

All p50, 50 runs.

### findings

**Round-trip costs ~10 ns.** `enrich` embeds a Loop into the richer Net
GADT; `melt` collapses it back to Loop. The 10 ns overhead is the GADT
pattern matching + reconstruction. Spread is high (68%) because the
larger GADT triggers more GC.

**Net adds ~7 ns over Loop.** Compare to `loopLift` at 0.9 ns — the Net
round-trip is 10× the cost. For hot paths, stay in Loop; use Net only
when you need the extra structure (parallel composition, bimonoid).

## Layer — free construction interface

### variant

```haskell
layerCompose n = run (go n :: Loop (,) (->) Int Int) 0
  where go 0 = Lift (+1); go i = Lift (+1) .> go (i-1)
```

### scaling data

| length | layerCompose |
|--------|-------------|
| 100    | 30.0 ns     |
| 500    | 30.1        |
| 1000   | 30.0        |
| 5000   | 39.1        |

All p50, 50 runs. Identical to `loopCompose` — `.>` and `(.)` on Loop
produce the same GADT structure.

## consolidated cost model

| component | operation | ns/call | vs direct |
|-----------|-----------|---------|-----------|
| direct | function call | 0.7 | 1× |
| Channel | assoc, slide | ~0 | noise floor |
| Traced Either | trace (iteration) | 13 | 19× |
| Traced (,) | trace (lazy knot) | 3.2 | 5× |
| Hyper | push | 1.0 | 1.4× |
| Hyper | nest (invoke) | 0.8 | 1.1× |
| Hyper | compose (.) | 18 | 26× |
| Hyper | runHyper | 12 | 17× |
| Hyper | lift + observe | 25 | 36× |
| Loop | Lift + run | 0.9 | 1.3× |
| Loop | Knot + run | 2.9 | 4× |
| Loop | compose (.) | 33 | 47× |
| Net | enrich + melt | 10 | 14× |
| Layer | compose (.>) | 33 | 47× |

## Process — Moore machine

### variants

```haskell
processScan = scan (P id (+) id)  -- running sum via Moore machine
processFold = fold (P id (+) id)  -- final value
directScan  = scanl' (+) 0        -- direct scanl'
directFold  = foldl' (+) 0        -- direct foldl'
```

### scaling data

| length | processScan | processFold | directScan | directFold |
|--------|------------|-------------|-----------|-----------|
| 100    | 31.3       | 11.3        | 10.4      | 6.3       |
| 500    | 10.1       | 10.1        | 9.8       | 7.3       |
| 1000   | 10.0       | 10.0        | 9.3       | 6.2       |
| 5000   | 9.9        | 9.8         | 11.4      | 6.0       |

All p50, 50 runs. ns/elem = total ÷ n.

### findings

**Moore machine adds ~4 ns over scanl'.** At 10 ns/elem, `scan (P id (+) id)`
is competitive with `scanl' (+) 0` at ~9 ns. The 1-4 ns overhead is the
Moore triple dispatch: inject, step × n, extract. At small n (100), the
overhead proportion is higher (31 ns) because the initial Moore allocation
dominates.

**fold vs scan: identical cost.** `fold` calls `last . scan` — the extra
`last` is negligible at scale.

**Fold is faster than Scan.** `directFold` at 6 ns vs `directScan` at 9 ns:
scan produces a full output list; fold only accumulates the final value.
The 3 ns difference is list construction overhead.

## Ends — channel ends

### variants

```haskell
endsOpen   n = N allocations of open :: Ends (->) () ()
endsClose  n = N calls to close (conjoint e) (companion e) ()
endsEmit   n = N calls to emit (companion e) (conjoint e) ()
endsCommit n = N calls to commit (conjoint e) (companion e) ()
```

All use unit ends `open :: Ends (->) () ()` which satisfy
`close (conjoint e) (companion e) = id` (yanking).

### scaling data

| length | endsOpen | endsClose | endsEmit | endsCommit |
|--------|---------|----------|----------|-----------|
| 100    | 1.3     | 2.9      | 1.3      | 2.9       |
| 500    | 0.8     | 2.4      | 0.8      | 2.3       |
| 1000   | 0.8     | 2.3      | 0.8      | 2.3       |
| 5000   | 0.7     | 2.2      | 0.7      | 2.1       |

All p50, 50 runs. ns/elem = total ÷ n.

### findings

**Open is free.** At ~0.9 ns, allocating a unit Ends pair is at the noise
floor. The `HasUnit () (->)` instance constructs two trivial closures.

**Emit is free; commit costs ~2.5 ns.** `emit` is rank-2 but resolves to a
known `In` at the call site — GHC eliminates the indirection. `commit`
carries the actual function application cost. The asymmetry reflects the
polarity: Out produces, In consumes.

**Close costs ~2.5 ns.** The full `close (conjoint e) (companion e) ()`
round-trip is the commit of the Out through the In — same as the commit
cost alone, since emit is zero-cost.

**One-sided channel ends are viable.** Peeling off just an `Out` or `In`
and using it independently adds ~0-2.5 ns overhead over direct function
application. The channel abstraction is essentially free in `(->)`.

## consolidated cost model

| component | operation | ns/call | vs direct |
|-----------|-----------|---------|-----------|
| direct | function call | 0.7 | 1× |
| Channel | assoc, slide | ~0 | noise floor |
| Traced Either | trace (iteration) | 13 | 19× |
| Traced (,) | trace (lazy knot) | 3.2 | 5× |
| Hyper | push | 1.0 | 1.4× |
| Hyper | nest (invoke) | 0.8 | 1.1× |
| Hyper | compose (.) | 18 | 26× |
| Hyper | runHyper | 12 | 17× |
| Hyper | lift + observe | 25 | 36× |
| Loop | Lift + run | 0.9 | 1.3× |
| Loop | Knot + run | 2.9 | 4× |
| Loop | compose (.) | 33 | 47× |
| Net | enrich + melt | 10 | 14× |
| Layer | compose (.>) | 33 | 47× |
| Process | scan (vs scanl') | 1-4 | 1.5–6× |
| Process | fold (vs foldl') | ~4 | ~1.7× |
| Ends | open | 0.9 | 1.3× |
| Ends | close | 2.5 | 3.6× |
| Ends | emit (one-sided) | 0.8 | 1.1× |
| Ends | commit (one-sided) | 2.4 | 3.4× |

## Words — resource acquisition + Either loop

### variants

```haskell
-- Loop Either (Kleisli IO): open → read loop → close
wordsLoop n = withSystemTempFile ... $ \fp h -> do
  writeNLines h n; hClose h
  runKleisli (run pipeline) fp  -- pipeline = openf .> readAll .> closef

-- Direct IO: open → read → close, same algorithm, no Loop
wordsDirect n = withSystemTempFile ... $ \fp h -> do
  writeNLines h n; hClose h
  h2 <- openFile fp ReadMode; go ...; hClose h2
```

### scaling data

| length | wordsLoop | wordsDirect |
|--------|----------|------------|
| 100    | 1860     | 1882       |
| 500    | 634      | 549        |
| 1000   | 469      | 361        |

All p50, 10 runs. ns/elem = total ÷ n. Spread 200-400% (file I/O variance).

### findings

**Loop adds no measurable overhead for I/O.** At 360-1900 ns/elem, both
variants are dominated by file system calls (open, write, read, close,
temp-file cleanup). The Loop Either construction adds <5% overhead —
invisible against the I/O noise floor.

## Kleisli IO — monadic overhead

### variants

```haskell
kLoopLift    n = N iterations of runKleisli (run (Lift (Kleisli pure . (+1)))) acc >>= ...
kLoopKnot    n = N iterations of runKleisli (run (Knot (Kleisli step))) () >>= ...
kHyperPush   n = Hyper push chain, lifted into Kleisli IO
kHyperCompose n = Hyper composition chain, lifted into Kleisli IO
kDirect      n = manual bind loop: step acc >>= go (i-1)
```

### scaling data

| length | kLoopLift | kLoopKnot | kHyperPush | kHyperCompose | kDirect |
|--------|----------|----------|-----------|-------------|--------|
| 100    | 3.3      | 25.8     | 1.7       | 90.4        | 1.3    |
| 500    | 1.0      | 25.4     | 0.9       | 88.5        | 2.3    |
| 1000   | 0.9      | 26.1     | 0.8       | 88.5        | 0.8    |
| 5000   | 0.9      | 25.2     | 0.7       | 94.7        | 0.8    |

All p50, 30 runs. ns/elem = total ÷ n.

### Kleisli tax

| operation | pure | Kleisli IO | factor | why |
|-----------|------|-----------|--------|-----|
| Loop Lift+run | 0.9 | 0.9 | 1× | newtype wrapper, no effect |
| Loop Knot (Either) | 13 | 25.6 | 2× | `>>=` per iteration |
| Hyper push | 1.0 | 1.0 | 1× | pure coincidence at this size |
| Hyper compose | 18 | 90 | 5× | `runArr` at each composed layer |
| Direct loop | 0.7 | 0.8 | 1× | baseline |

### findings

**The tax is structural, not uniform.** Components that just wrap values
(Lift, push) pay nothing — the Kleisli newtype is zero-cost. Components
with internal control flow (Either trace) pay 2× for the monadic bind
per iteration. Components with layered composition (Hyper `(.)`) pay
5× because each composed layer adds a `runArr` call inside the monad.

**Kleisli IO is still fast enough.** Even at 5×, Hyper compose in
Kleisli (90 ns) is competitive with many real-world operations. The
Loop Either trace at 26 ns is 2× pure but still O(1) per element.
For I/O-bound workloads (wordsLoop at 360-1900 ns), the Kleisli tax
is irrelevant — the monadic overhead is dwarfed by syscalls.

## consolidated cost model

| component | operation | ns/call | vs direct |
|-----------|-----------|---------|-----------|
| direct | function call | 0.7 | 1× |
| Channel | assoc, slide | ~0 | noise floor |
| Traced Either | trace (iteration) | 13 | 19× |
| Traced (,) | trace (lazy knot) | 3.2 | 5× |
| Hyper | push | 1.0 | 1.4× |
| Hyper | nest (invoke) | 0.8 | 1.1× |
| Hyper | compose (.) | 18 | 26× |
| Hyper | runHyper | 12 | 17× |
| Hyper | lift + observe | 25 | 36× |
| Loop | Lift + run | 0.9 | 1.3× |
| Loop | Knot + run | 2.9 | 4× |
| Loop | compose (.) | 33 | 47× |
| Net | enrich + melt | 10 | 14× |
| Layer | compose (.>) | 33 | 47× |
| Process | scan (vs scanl') | 1-4 | 1.5–6× |
| Process | fold (vs foldl') | ~4 | ~1.7× |
| Ends | open | 0.9 | 1.3× |
| Ends | close | 2.5 | 3.6× |
| Ends | emit (one-sided) | 0.8 | 1.1× |
| Ends | commit (one-sided) | 2.4 | 3.4× |
| Kleisli | Knot Either trace | 26 | 37× |
| Kleisli | Hyper compose | 90 | 129× |
| Words IO | Loop Either pipeline | 360-1900 | I/O-bound |

