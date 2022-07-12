{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module LLVM.Rules.Plugin ( Plugin, makePlugin ) where

import Relude hiding ( Type )
import Foreign hiding ( void, new )
import Foreign.C
import Language.C.Inline.Unsafe qualified as Cxx
import Language.C.Inline.Cpp ( context, bsCtx, vecCtx, cppCtx, cppTypePairs, include, using )
import Control.Exception
import Data.Vector.Storable as Vec ( thaw )
import Data.Vector.Storable.Mutable as Vec ( replicate, read, write )
import LLVM.Rules.Rule

data Value
data Instr
data CallInstr
data IRBuilder

context $ bsCtx <> vecCtx <> cppCtx <> cppTypePairs
  [ ("Instr", [t| Instr |])
  , ("Value", [t| Value |])
  , ("Type",  [t| Type  |])
  , ("CallBase",  [t| CallInstr |])
  , ("Builder",  [t| IRBuilder |])
  ]

include "llvm/IR/IRBuilder.h"
include "llvm/IR/BasicBlock.h"
include "llvm/IR/Instruction.h"
include "llvm/IR/Intrinsics.h"
include "llvm/IR/InstrTypes.h"
include "llvm/IR/Instructions.h"
include "llvm/IR/Operator.h"
include "llvm/IR/Function.h"

using "namespace llvm"
using "Instr = Instruction"
using "Builder = IRBuilder<>"

applyRule :: Rule -> Ptr Instr -> IO Bool
applyRule (Rule label src dst nvars) root = isRight <$> try @Mismatch do
  env <- Vec.replicate nvars nullPtr

  let
    bind :: Var -> Ptr Value -> IO ()
    bind (MkVar i) new = do
      old <- read env i
      when (old /= nullPtr && new /= old) mismatch
      write env i new

    load :: Var -> IO (Ptr Value)
    load (MkVar i) = do
      ptr <- read env i
      when (ptr == nullPtr) $ error "unbound variable"
      pure ptr

  let
    match :: AST -> Ptr Value -> IO ()
    match (Var var)     val = bind var val
    match (Add lhs rhs) val = do
      (lhs_, rhs_) <- asBin [Cxx.pure| unsigned { Instr::Add } |] val
      match lhs lhs_
      match rhs rhs_
    match (Sub lhs rhs) val = do
      (lhs_, rhs_) <- asBin [Cxx.pure| unsigned { Instr::Sub } |] val
      match lhs lhs_
      match rhs rhs_
    match (Mul lhs rhs) val = do
      (lhs_, rhs_) <- asBin [Cxx.pure| unsigned { Instr::Mul } |] val
      match lhs lhs_
      match rhs rhs_
    match (Intrinsic name args _) val = do
      ccheck =<< [Cxx.block| bool {
        auto op = dyn_cast<CallBase>($(Value* val));
        if (!op) return false;
        auto fn = op->getCalledFunction();
        if (!fn) return false;
        auto id = fn->getIntrinsicID();
        if (!id) return false;
        return Intrinsic::getBaseName(id).equals({$bs-ptr:name, (size_t) $bs-len:name});
      } |]

      let argsCount = fromIntegral $ length args
      ccheck =<< [Cxx.exp| bool { cast<CallBase>($(Value* val))->arg_size() == $(unsigned argsCount) } |]

      for_ (zip [0..] args) \(idx, arg) ->
        match arg =<< [Cxx.exp| Value* { cast<CallBase>($(Value* val))->getArgOperand($(unsigned idx)) } |]

    asInstr :: Ptr Value -> IO (Ptr Instr)
    asInstr expr = do
      instr <- [Cxx.exp| Instr* { dyn_cast<Instr>($(Value* expr)) } |]
      check $ instr /= nullPtr
      pure instr

    asBin :: CUInt -> Ptr Value -> IO (Ptr Value, Ptr Value)
    asBin opcode expr = do
      instr <- asInstr expr
      ccheck =<< [Cxx.exp| bool { $(Instr* instr)->getOpcode() == $(unsigned opcode) } |]
      lhs <- [Cxx.exp| Value* { $(Instr* instr)->getOperand(0) } |]
      rhs <- [Cxx.exp| Value* { $(Instr* instr)->getOperand(1) } |]
      pure (lhs, rhs)


  match src =<< [Cxx.exp| Value* { cast<Value>($(Instr* root)) } |]

  builder <- [Cxx.exp| Builder* { new IRBuilder($(Instr* root)) } |]

  let
    build :: AST -> IO (Ptr Value)
    build (Var var) = load var
    build (Add lhs rhs) = do
      lhs' <- build lhs
      rhs' <- build rhs
      [Cxx.exp| Value* { $(Builder* builder)->CreateAdd($(Value* lhs'), $(Value* rhs')) } |]
    build (Sub lhs rhs) = do
      lhs' <- build lhs
      rhs' <- build rhs
      [Cxx.exp| Value* { $(Builder* builder)->CreateSub($(Value* lhs'), $(Value* rhs')) } |]
    build (Mul lhs rhs) = do
      lhs' <- build lhs
      rhs' <- build rhs
      [Cxx.exp| Value* { $(Builder* builder)->CreateMul($(Value* lhs'), $(Value* rhs')) } |]
    build (Intrinsic name args extr) = do
      argsVs <- traverse build args
      argsTs <- traverse getType argsVs
      vargs_ <- thaw $ fromList argsVs
      targs_ <- thaw $ fromList $ extr argsTs
      [Cxx.block| Value* {
        auto id = Function::lookupIntrinsicID({$bs-ptr:name, (size_t) $bs-len:name});
        assert(id != Intrinsic::not_intrinsic); // TODO report error in haskell-land
        return $(Builder* builder)->CreateIntrinsic(
          id,
          {$vec-ptr:(Type** targs_), (size_t) $vec-len:targs_},
          {$vec-ptr:(Value** vargs_), (size_t) $vec-len:vargs_}
        );
      } |]

  new <- build dst

  [Cxx.exp| void { delete $(Builder* builder); } |]
  [Cxx.exp| void { $(Instr* root)->replaceAllUsesWith($(Value* new)); } |]

  putTextLn $ "; Rule " <> label <> " fired"

getType :: Ptr Value -> IO (Ptr Type)
getType val = [Cxx.exp| Type* { $(Value* val)->getType() } |]


data Mismatch = Mismatch
  deriving stock Show
  deriving anyclass Exception

mismatch :: IO a
mismatch = throwIO Mismatch

check :: Bool -> IO ()
check = unless ?? mismatch

ccheck :: CBool -> IO ()
ccheck = check . toBool


type Plugin = Ptr Instr -> IO Bool

makePlugin :: [Rule] -> Plugin
makePlugin rules instr = anyM (`applyRule` instr) rules
