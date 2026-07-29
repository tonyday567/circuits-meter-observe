# circuits-meter-observe basics

Calibration data and ground-floor probe results. Apple M4 Pro, GHC 9.14.1,
`CLOCK_MONOTONIC_RAW`, `circuits-meter` bracket.

## calibration

The measurement floor is the `meterAction timeX` bracket: `start` → `stop`
around an empty body. Measured via `probe --bracket`:

```
bracket overhead: 42 ns  (two clock_gettime + Kleisli wrapper + NFData force)
```

Subtract from all snippet timings to get true per-snippet cost. For operations
with per-element times below ~1 ns, use longer inputs so the bracket is
negligible.

Clock resolution floor (from C baseline, `other/c_baseline.c`): 0.33 ns.
Haskell empty loop floor (from `examples/baseline-analytics.md`): 0.60 ns/iter.
These are the asymptotes — nothing in Haskell gets faster than the loop overhead.

## ground-floor probes

All measurements: p10 quantile, 100 runs, bracket included unless noted.

### function application

| snippet | length | total | ns/elem | note |
|---------|--------|-------|---------|------|
| id | — | 42 ns | — | bracket floor — id costs nothing |
| closure | — | 42 ns | — | closure creation absorbed by bracket |
| fold | 1000 | 708 ns | 0.71 | tight loop, add+subtract, unboxed Int# |
| fold | 10000 | 6292 ns | 0.63 | asymptote, near empty-loop floor |
| arith | 1000 | 959 ns | 0.96 | multiply per iteration, ~0.3 ns more than add |

Interpretation: a Haskell `NOINLINE` function call costs ~1.4 ns (see
`baseline-analytics.md`), but with p10 over 100 runs the noise floor is
dominated by the bracket. The loop asymptote (0.63 ns/elem) matches the
empty-loop floor of 0.60 ns/iter — our fold body costs essentially nothing.

### list operations

| snippet | length | total | ns/elem | note |
|---------|--------|-------|---------|------|
| cons | 1000 | 4375 ns | 4.4 | build [1..n] with (:), ~4.4 ns/cell |
| sum | 1000 | 5750 ns | 5.8 | cons + foldl', ~1.4 ns overhead over cons |
| append | 1000 | 8750 ns | 8.8 | xs ++ xs, double the cons cost |
| nub | 500 | 3.1 ms | — | O(n²), quadratic reference |

The cons cost (4.4 ns/cell) decomposes into spine pointer-chase (2.11 ns) +
element access (1.42 ns) per `baseline-analytics.md`. Sum adds the fold
overhead. Append doubles the cons work. Nub is the quadratic reference —
everything else should be linear.

### substrate primitives

| snippet | length | total | note |
|---------|--------|-------|------|
| loop | 500 | 3.8 µs | IORef counting loop, per-element ~7.6 ns |
| stopwatch | 500 | 0.8 µs | pipeline overhead: start → carry → lap → stop |
| meter-action | 1000 | 2.6 µs | meterAction bracket around pure sum |

## methodology: three instruments

The timer says *that* something is slow. Core says *how the loop is shaped*.
Allocation + GC counts say *why the tail exists*.

From `examples/baseline-analytics.md`: `polySum Int` had a tight p50 but
reproducible p99 tail. The timer found the tail. Core proved the loop was
fine (dictionary hoisted out). Only the space/GC meter caught the cause:
double the minor GCs from forced list boxing.

**Distribution matters, not just averages.** Use p10 (clean proxy — least
contamination from GC/scheduler noise) and p90/p99 (tail detection). A
single p50 can hide a fat tail.

## scaling checks

`probe --lengths 100,200,500,1000,2000,5000,10000` runs a snippet at
multiple sizes and reports ns/elem per size. If per-element cost isn't
constant across sizes, something is nonlinear:

- **Space leak**: timings grow faster than linear as n increases
- **GC threshold**: step change when allocation crosses a nursery boundary
- **Cache effect**: ~constant below L1, linear above

The `scalingSummary` reports min/max ns/elem and spread. Spread > 50%
warrants investigation. The asymptote at large n tells you the true per-unit
cost after amortising fixed overheads.

## running

```bash
# List snippets
cabal run probe -- --list

# Single snippet, single length
cabal run probe -- --snippet fold --runs 100 --length 10000

# Multi-length scaling check
cabal run probe -- --snippet sumTail --runs 50 --lengths 200,500,1000,2000,5000,10000 --record

# Check against golden
cabal run probe -- --snippet sumTail --runs 50 --lengths 200,500,1000,2000,5000,10000

# Clean proxy (p10)
cabal run probe -- --snippet fold --runs 100 --quantile 0.1 --length 10000

# Bracket overhead
cabal run probe -- --snippet bracket --runs 100 --length 1
```

## recursion catalog

The probe includes a catalog of recursion patterns ported from Perf.Algos
(v0.12). Each variant implements the same operation (sum, length, recurse) in
a different style. The multi-length scaling check reveals the constant factors.

All measurements: p50, 50 runs, n=10000 asymptote, list built via `buildList`
(tail-recursive cons). Times include the list construction overhead (~4 ns/elem).

### sum variants

| snippet | ns/elem | spread | style |
|---------|---------|--------|-------|
| sumMono | 5.9 | 5% | `foldl' (+) 0` on `[Int]` — monomorphic, the winner |
| sumPoly | 5.4 | 57% | `foldl' (+) 0` on `Num a => [a]` — dictionary eliminated by spec |
| sumSum | 5.6 | 56% | `Prelude.sum` — same ballpark |
| sumFoldr | 14.7 | 78% | `foldr (+) 0` — stack, GC interaction |
| sumCo | 15.3 | 70% | co-routine (non-tail) — builds stack, worse |
| sumTail | 18.3 | 60% | tail recursive with `$!` — 3× slower than foldl' |
| sumTailLazy | 19.2 | 56% | tail recursive, lazy accumulator — thunk buildup, worst |

Key findings:

- **foldl' wins.** `sumMono` at 5.9 ns/elem with 5% spread is the fastest and
  most predictable. Manual tail recursion with `$!` (`sumTail`) is 3× slower
  at 18.3 ns/elem. GHC's `foldl'` is highly optimised; hand-written
  tail recursion is not.

- **Specialisation eliminates dictionary overhead.** `sumPoly` (polymorphic)
  and `sumMono` (monomorphic) converge to the same speed because GHC
  specialises the `Num a` dictionary at the `[Int]` call site. The wider
  spread (57% vs 5%) is GC interaction, not dictionary dispatch.

- **Stack recursion has predictable overhead.** `sumCo` and `sumFoldr` at
  15 ns/elem pay ~9 ns/elem extra for stack frames. The wider spread (70-78%)
  reflects GC pauses during stack allocation.

- **Lazy accumulator = space leak.** `sumTailLazy` is the worst performer
  and the only one where allocation (thunks) dominates compute. This is the
  test case for allocX vs allocGC.

### length variants

| snippet | ns/elem | spread | style |
|---------|---------|--------|-------|
| lengthPrelude | ~6 | — | `Prelude.length` — GHC builtin |
| lengthTail | ~6 | — | tail recursive, `$!` strict spine |
| lengthFoldr | ~6 | — | `foldr (\_ n -> n + 1) 0` |

Length variants converge to the same speed — GHC produces identical Core for
all three. The operation is too simple for style to matter.

### recursion (no list)

| snippet | ns/elem | style |
|---------|---------|-------|
| recurseTail | 0.63 | tail recursive counter — hits empty-loop floor |
| recurseCo | ~1 | co-routine (non-tail) — stack overhead |

`recurseTail` at 0.63 ns/elem matches the empty-loop floor from the
baseline analytics. This is the cheapest possible loop in Haskell —
decrement, compare-zero, jump. No allocation, no stack, unboxed Int#.

### allocation metering

The thunk-buildup case (`sumTailLazy`) exposes the limits of GHC's
`allocated_bytes` counter:

| method | time | alloc | note |
|--------|------|-------|------|
| allocX (no GC) | 14 ms | 8.3 MB | counter captured via natural GCs — correct |
| allocGC (GC both ends) | 3.9 ms | 2.3 KB | stop GC freed thunks before measurement — wrong |
| GC-start + allocX | 2.4 µs | 0 B | nursery fit, counter didn't advance — wrong |

`allocX` works when the computation triggers GCs (as 100k element lists do).
For small allocations, neither meter works well. The `allocated_bytes`
counter measures heap *growth*, not total allocation — ephemeral allocations
that are GC'd within the interval are invisible.

Three instruments: timer (p50), Core (loop shape), space meter (allocX for
large allocations, allocGC for per-stage relative comparisons). None is
sufficient alone.
