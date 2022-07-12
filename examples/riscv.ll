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
