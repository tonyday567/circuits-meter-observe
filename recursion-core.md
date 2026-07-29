# recursion core — GHC Core for the sum variants

Core dump from `cabal build probe --ghc-options="-ddump-simpl -dsuppress-all
-dno-suppress-type-signatures"`, GHC 9.14.1 -O1. Source: `app/Probe.hs`.

## tier 1 — fast (~5-6 ns/elem)

### sumMono (5.9 ns, 5% spread)

Gets full worker/wrapper. The loop is unboxed `Int# -> Int#` with `(+#)`.
No boxing, no dictionary, no allocation in the hot path.

```haskell
Rec {
$wgo4_r9qX :: [Int] -> Int# -> Int#
$wgo4_r9qX
  = \ (ds_s945 :: [Int]) (ww_s948 :: Int#) ->
      case ds_s945 of {
        [] -> ww_s948;
        : y_a8Yu ys_a8Yv ->
          case y_a8Yu of { I# y1_a7vy ->
          $wgo4_r9qX ys_a8Yv (+# ww_s948 y1_a7vy)
          }
      }
end Rec }

$wsumMono :: [Int] -> Int#
$wsumMono = \ (xs_s94d :: [Int]) -> $wgo4_r9qX xs_s94d 0#

sumMono :: [Int] -> Int
sumMono
  = \ (xs_s94d :: [Int]) ->
      case $wsumMono xs_s94d of ww_s9eF { __DEFAULT -> I# ww_s9eF }
```

Wrap/unwrap `I#` at boundaries only. Loop body: unbox cons, `(+#)`, tail jump.

### sumPoly (5.4 ns, 57% spread)

GHC hoists the dictionary out of the loop, forces the accumulator at each step,
marks the loop as `joinrec` (guaranteed tail-call join point):

```haskell
sumPoly :: forall a. Num a => [a] -> a
sumPoly
  = \ (@a_a5CA) ($dNum_a5CB :: Num a_a5CA) ->
      let {
        k_s8rf :: a_a5CA -> a_a5CA -> a_a5CA
        k_s8rf = + $dNum_a5CB } in        -- dictionary hoisted out
      let {
        z0_s8rh :: a_a5CA
        z0_s8rh = fromInteger $dNum_a5CB lvl14_r9qn } in
      \ (xs_a7oe :: [a_a5CA]) ->
        joinrec {
          go2_a8Yq :: [a_a5CA] -> a_a5CA -> a_a5CA
          go2_a8Yq (ds_a8Yr :: [a_a5CA]) (eta_B0 :: a_a5CA)
            = case ds_a8Yr of {
                [] -> eta_B0;
                : y_a8Yu ys_a8Yv ->
                  case eta_B0 of z_a7oi { __DEFAULT ->    -- force accumulator
                  jump go2_a8Yq ys_a8Yv (k_s8rf z_a7oi y_a8Yu)
                  }
              }; } in
        jump go2_a8Yq xs_a7oe z0_s8rh
```

Boxed `Int` accumulator, dictionary `+`, but `joinrec` + hoisting keeps it fast.
The 57% spread is GC interaction on the boxed accumulator — not dictionary overhead.

### sumSum (5.6 ns, 56% spread)

Identical Core to sumPoly. `Prelude.sum = foldl' (+) 0` compiles to the same
`joinrec` with dictionary hoisting. The timing confirms it.

## tier 2 — medium (~15 ns/elem)

### sumFoldr (14.7 ns, 78% spread)

Standard foldr. Non-tail call builds O(n) heap closures:

```haskell
sumFoldr :: forall a. Num a => [a] -> a
sumFoldr
  = \ (@a_a5D6) ($dNum_a5D7 :: Num a_a5D6) ->
      let { k_a8Yn = + $dNum_a5D7 } in
      let { z_a8Yo = fromInteger $dNum_a5D7 lvl14_r9qn } in
      letrec {
        go2_s9gx :: [a_a5D6] -> a_a5D6
        go2_s9gx
          = \ (ds_a8Yr :: [a_a5D6]) ->
              case ds_a8Yr of {
                [] -> z_a8Yo;
                : y_a8Yu ys_a8Yv -> k_a8Yn y_a8Yu (go2_s9gx ys_a8Yv)
                                           -- NOT a tail call — go2 result is arg to k
              }; } in
      go2_s9gx
```

### sumCo (15.3 ns, 70% spread)

Direct non-tail recursion. Same pattern:

```haskell
Rec {
sumCo :: forall a. Num a => [a] -> a
sumCo
  = \ (@a_a5Dn) ($dNum_a5Do :: Num a_a5Dn) (ds_d6Mb :: [a_a5Dn]) ->
      case ds_d6Mb of {
        [] -> fromInteger $dNum_a5Do lvl14_r9qn;
        : x6_a5by xs_a5bz ->
          + $dNum_a5Do x6_a5by (sumCo $dNum_a5Do xs_a5bz)
                                -- non-tail, stack per element
      }
end Rec }
```

Both build O(n) closures. The ~9 ns extra over foldl' is closure allocation + GC.

## tier 3 — slow (~18-19 ns/elem)

### sumTail (18.3 ns, 60% spread)

`letrec` (NOT `joinrec`), dictionary NOT hoisted, `$!` on list spine but
accumulator is lazy:

```haskell
sumTail :: forall a. Num a => [a] -> a
sumTail
  = \ (@a_a5EC) ($dNum_a5ED :: Num a_a5EC) ->
      letrec {
        go2_s8qx :: a_a5EC -> [a_a5EC] -> a_a5EC
        go2_s8qx
          = \ (acc_a5bm :: a_a5EC) (ds_d6Mp :: [a_a5EC]) ->
              case ds_d6Mp of {
                [] -> acc_a5bm;
                : x6_a5bo xs_a5bp ->
                  case xs_a5bp of vx_a6TB { __DEFAULT ->   -- $! forces spine
                  go2_s8qx (+ $dNum_a5ED x6_a5bo acc_a5bm) vx_a6TB
                  }                                         -- dictionary NOT hoisted
              }; } in
      go2_s8qx (fromInteger $dNum_a5ED lvl14_r9qn)
```

### sumTailLazy (19.2 ns, 56% spread)

Same but no forcing at all — accumulator thunks + lazy spine:

```haskell
sumTailLazy :: forall a. Num a => [a] -> a
sumTailLazy
  = \ (@a_a5DG) ($dNum_a5DH :: Num a_a5DG) ->
      letrec {
        go2_s8qz :: a_a5DG -> [a_a5DG] -> a_a5DG
        go2_s8qz
          = \ (acc_a5bu :: a_a5DG) (ds_d6Mi :: [a_a5DG]) ->
              case ds_d6Mi of {
                [] -> acc_a5bu;
                : x6_a5bw xs_a5bx ->
                  go2_s8qz (+ $dNum_a5DH x6_a5bw acc_a5bu) xs_a5bx
                  -- lazy accumulator, lazy spine
              }; } in
      go2_s8qz (fromInteger $dNum_a5DH lvl14_r9qn)
```

## why the 3× gap

Three factors, roughly in order of impact:

1. **`joinrec` vs `letrec`** — join points get better codegen. GHC knows the
   recursive call is always a tail jump, so it can avoid closure allocation on
   each iteration. `letrec` means mutual recursion is possible, so GHC
   allocates a closure.

2. **Dictionary hoisting** — sumPoly lifts `k = + $dNum` before the loop.
   sumTail passes `$dNum` on every recursive call, requiring an extra
   indirection per iteration to reach `+`.

3. **Wrong strictness target** — sumTail's `$! xs` forces the list spine but
   NOT the accumulator. The `x + acc` thunk builds up just like sumTailLazy.
   foldl' forces the accumulator, which is what you actually want. The `$!`
   is on the wrong argument.

## surprise: polymorphic ≈ monomorphic

sumPoly (polymorphic, dictionary dispatch) at 5.4 ns is marginally faster than
sumMono (monomorphic, unboxed Int#) at 5.9 ns. The dictionary is a known
static value — the CPU branch predictor handles the indirect call perfectly.
The unboxing in sumMono saves nothing because the `I#` constructor is stripped
and rewrapped at the loop boundary anyway. The boxed accumulator in sumPoly
costs less than the unbox/rewrap dance in sumMono.
