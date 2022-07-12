{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Plugin ( rulesPlugin ) where

import Relude
import LLVM.Rules.Rule
import LLVM.Rules.Plugin

foreign export ccall rulesPlugin :: Plugin

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
