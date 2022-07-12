# [Early WIP] DSL for peephole optimisations on LLVM IR

## Example

```haskell
rulesPlugin :: Plugin
rulesPlugin = makePlugin
  [ rule "add-sub"
      \a b s -> vsub (vadd a b s) b s ->> a
  , rule "mul-left-dist"
      \a b c s -> vadd (vmul a c s) (vmul b c s) s ->> vmul (vadd a b s) c s
  , rule "min-min-right"
      \a b s -> vmin a (vmin a b s) s ->> vmin a b s
  ]

vadd a b s = intrinsic "llvm.riscv.vadd" [a, b, s] id
vsub a b s = intrinsic "llvm.riscv.vsub" [a, b, s] id
vmul a b s = intrinsic "llvm.riscv.vmul" [a, b, s] id
vmin a b s = intrinsic "llvm.riscv.vmin" [a, b, s] id
```

```
> cat riscv.ll
declare <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, i64)
declare <vscale x 32 x i8> @llvm.riscv.vsub.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, i64)
declare <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, i64)
declare <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8>, <vscale x 32 x i8>, i64)

define <vscale x 32 x i8> @add_sub(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 noundef %2) {
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2)
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vsub.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %4, <vscale x 32 x i8> %1, i64 %2)
  ret <vscale x 32 x i8> %5
}

define <vscale x 32 x i8> @mul_dist(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, <vscale x 32 x i8> %2, i64 %3) {
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %2, i64 %3)
  %6 = tail call <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %1, <vscale x 32 x i8> %2, i64 %3)
  %7 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %5, <vscale x 32 x i8> %6, i64 %3)
  ret <vscale x 32 x i8> %7
}

define <vscale x 32 x i8> @min_min(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2) {
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2)
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %4, i64 %2)
  ret <vscale x 32 x i8> %5
}
```

```
> opt -O3 riscv.ll -S
<...>

define <vscale x 32 x i8> @add_sub(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 noundef %2) local_unnamed_addr #1 {
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2)
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vsub.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %4, <vscale x 32 x i8> %1, i64 %2)
  ret <vscale x 32 x i8> %5
}

define <vscale x 32 x i8> @mul_dist(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, <vscale x 32 x i8> %2, i64 %3) local_unnamed_addr #1 {
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %2, i64 %3)
  %6 = tail call <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %1, <vscale x 32 x i8> %2, i64 %3)
  %7 = tail call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %5, <vscale x 32 x i8> %6, i64 %3)
  ret <vscale x 32 x i8> %7
}

define <vscale x 32 x i8> @min_min(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2) local_unnamed_addr #1 {
  %4 = tail call <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2)
  %5 = tail call <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %4, i64 %2)
  ret <vscale x 32 x i8> %5
}

<...>
```

```
> opt --load-pass-plugin ../build/rulesPlugin.so --passes="default<O3>,function(rules)" riscv.ll -S
; Rule add-sub fired
; Rule mul-left-dist fired
; Rule min-min-right fired

<...>

define <vscale x 32 x i8> @add_sub(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 noundef %2) local_unnamed_addr #1 {
  ret <vscale x 32 x i8> %0
}

define <vscale x 32 x i8> @mul_dist(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, <vscale x 32 x i8> %2, i64 %3) local_unnamed_addr #1 {
  %5 = call <vscale x 32 x i8> @llvm.riscv.vadd.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %3)
  %6 = call <vscale x 32 x i8> @llvm.riscv.vmul.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %5, <vscale x 32 x i8> %2, i64 %3)
  ret <vscale x 32 x i8> %6
}

define <vscale x 32 x i8> @min_min(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2) local_unnamed_addr #1 {
  %4 = call <vscale x 32 x i8> @llvm.riscv.vmin.nxv32i8.nxv32i8.i64(<vscale x 32 x i8> %0, <vscale x 32 x i8> %1, i64 %2)
  ret <vscale x 32 x i8> %4
}

<...>
```

(tested with llvm-14)
