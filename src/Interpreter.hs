module Interpreter where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lambda

data Expr = Assign String Lambda
          | Eval Lambda
          deriving (Show)

resolveMacros :: [Expr] -> [Expr]
resolveMacros = resolveMacros' Map.empty

resolveMacros' :: Map.Map String Lambda -> [Expr] -> [Expr]
resolveMacros' _ [] = []
resolveMacros' macros (Assign name t:rest) = resolveMacros' (Map.insert name ( substituteMacro t macros Set.empty ) macros) rest
resolveMacros' macros (Eval t:rest) = Eval (substituteMacro t macros Set.empty) : resolveMacros' macros rest

substituteMacro :: Lambda -> Map.Map String Lambda -> Set.Set String -> Lambda
substituteMacro (Var x) macroMap _ = Var x
substituteMacro (Abs x t1) macroMap visitedMacros = Abs x (substituteMacro t1 macroMap visitedMacros)
substituteMacro (App t1 t2) macroMap visitedMacros = App (substituteMacro t1 macroMap visitedMacros) (substituteMacro t2 macroMap visitedMacros)
substituteMacro (Macro name) macroMap visitedMacros = if Set.member name visitedMacros
                                                      then error "Macro recursion detected"
                                                      else case Map.lookup name macroMap of
                                                          Nothing -> error $ "Macro " ++ name ++ " not found"
                                                          Just t -> substituteMacro t macroMap (Set.insert name visitedMacros)

run :: [Expr] -> [Lambda]
run exprs = fmap tryReduce
           $ fmap (\(Eval t) -> t)
           $ filter (\case Eval _ -> True; _ -> False)
           $ resolveMacros exprs
