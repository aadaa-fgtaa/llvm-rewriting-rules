{-# LANGUAGE UndecidableInstances #-}

module LLVM.Rules.Rule where

import Relude hiding ( Type )
import Foreign ( Storable, Ptr )
import Data.HashSet qualified as HashSet

data Type

type OverloadedTypeExtractor = [Ptr Type] -> [Ptr Type]

newtype Var = MkVar Int
  deriving newtype (Storable, Eq, Ord, Hashable)


data AST
  = Var Var
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Intrinsic ByteString [AST] OverloadedTypeExtractor
  deriving stock Generic

freeVars :: AST -> HashSet Var
freeVars (Var var) = [var]
freeVars (Add lhs rhs) = freeVars lhs <> freeVars rhs
freeVars (Sub lhs rhs) = freeVars lhs <> freeVars rhs
freeVars (Mul lhs rhs) = freeVars lhs <> freeVars rhs
freeVars (Intrinsic _ args _) = foldMap freeVars args


type LLVM = Fresh AST

add, sub, mul :: LLVM -> LLVM -> LLVM
add = liftA2 Add
sub = liftA2 Sub
mul = liftA2 Mul

intrinsic :: ByteString -> [LLVM] -> OverloadedTypeExtractor -> LLVM
intrinsic a b t = (\x -> Intrinsic a x t) <$> sequence b


data Rule = UnckeckedRule { name :: Text, lhs :: AST, rhs :: AST, varsCount :: Int }

pattern Rule :: Text -> AST -> AST -> Int -> Rule
pattern Rule name lhs rhs varsCount <- UnckeckedRule name lhs rhs varsCount
{-# COMPLETE Rule #-}


class Ruleify v x | v -> x where
  ruleify :: v -> Fresh x

instance Ruleify (LLVM, LLVM) (AST, AST) where
  ruleify (f, g) = liftA2 (,) f g

instance (a ~ LLVM, Ruleify b r) => Ruleify (a -> b) r where
  ruleify f = do
    v <- fresh
    ruleify $ f $ pure $ Var $ MkVar v


rule :: Ruleify llvm (AST, AST) => Text -> llvm -> Rule
rule name body = if freeVars rhs `HashSet.isSubsetOf` freeVars lhs
  then UnckeckedRule name lhs rhs len
  else error "unbound variable in rhs"
  where
    ((lhs, rhs), len) = runFresh $ ruleify body


infix 1 ->>
(->>) :: a -> b -> (a, b)
(->>) = (,)


newtype Fresh a = Fresh { unFresh :: State Int a }
  deriving newtype (Functor, Applicative, Monad)

runFresh :: Fresh a -> (a, Int)
runFresh = usingState 0 . unFresh

fresh :: Fresh Int
fresh = Fresh $ state $ id &&& (+ 1)
