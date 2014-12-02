{-# LANGUAGE OverloadedStrings #-}
module AvailableExpr where

import qualified Data.Set as S
import Data.List
import Data.String
import Debug.Trace

data Expr = 
          AEin Int
          | AEout Int
          | Set (S.Set (String, Aexpr))
          | Diff Expr Expr
          | U Expr Expr
          | Join Expr Expr 
          | Eqq Expr Expr
          deriving Eq

_s xs = Set $ S.fromList xs


instance Show Expr where
  show (AEin i) = "AEin(" ++ show i ++ ")"
  show (AEout i) = "AEout(" ++ show i ++ ")"
  show (Set s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"
  show (Diff e1 e2) = show e1 ++ " \\ " ++ show e2
  show (U e1 e2) = show e1 ++ " U " ++ show e2
  show (Join e1 e2) = show e1 ++ " Join " ++ show e2
  show (Eqq e1 e2) = show e1 ++ "=" ++ show e2

ppExpr :: [Expr] -> String
ppExpr = intercalate "\n" . map show

ppResult (ins, outs) = ppAsEqs AEin ins ++ "\n\n" ++ ppAsEqs AEout outs ++ "\n"
                           where
                             ppAsEqs ctor es = intercalate "\n" $ map (\(e,i) -> show (ctor i) ++ "=" ++ show e) $ zip es [1..]

e1 .\ e2 = Diff e1 e2

infix 1 .=.

i .=. s = Eqq i s

data Aexpr = Mul Aexpr Aexpr
           | Add Aexpr Aexpr
           | Div Aexpr Aexpr
           | Minus Aexpr Aexpr
           | Lit Int
           | Var String
             deriving (Eq, Ord)

instance IsString Aexpr where
  fromString a = Var a

instance Show Aexpr where
  show (Mul e1 e2) = show e1 ++ "*" ++ show e2
  show (Add e1 e2) = show e1 ++ "+" ++ show e2
  show (Div e1 e2) = show e1 ++ "/" ++ show e2
  show (Minus e1 e2) = show e1 ++ "-" ++ show e2
  show (Lit i) = show i 
  show (Var v) = show v
  
instance Num Aexpr where
  (*) = Mul
  (+) = Add
  (-) = Minus

vars = ["x", "y", "a", "b"] -- "Var" set
aExpr = [Add (Var "a") (Var "b"),
         Mul (Var "a") (Var "b"),
         Add (Var "a") (Lit 1)]

a `inExpr` (Mul e1 e2)  = (a `inExpr` e1) || (a `inExpr` e2)
a `inExpr` (Add e1 e2)  = (a `inExpr` e1) || (a `inExpr` e2)
a `inExpr` (Lit _)  = False
a `inExpr` (Var x) | a == x  = True
                   | otherwise = False


kill a =  _s [(v,e) | v <- vars,  e <- aExpr, a `inExpr` e] `U` _s [(a, e) | e <- aExpr]

-- use _s to make a set from list of pairs

eqs = [ AEin  1 .=. _s [] ,
        AEin  2 .=. AEout 1,
        AEin  3 .=. AEout 2 `Join` (AEout 5),
        AEin  4 .=. AEout 3,
        AEin  5 .=. AEout 4,
        
        AEout 1 .=. AEin  1 .\ kill "x" `U` _s [("x", "a" + "b")],
        AEout 2 .=. AEin  2 .\ kill "y" `U` _s [("y", "a" * "b")],
        AEout 3 .=. AEin  3,
        AEout 4 .=. AEin  4 .\ kill "a",
        AEout 5 .=. AEin  5 .\ kill "x" `U` _s [("x", "a" + "b")]
      ]

-- TODO add ordering!!!
getDefs eqs = ([e1 | Eqq (AEin _) e1 <- eqs], [e2 | Eqq (AEout _) e2 <- eqs])

substRD v rd s@(Set _) = s
substRD v rd (Diff e1 e2) = Diff (substRD v rd e1) (substRD v rd e2)
substRD v rd (U e1 e2) = U (substRD v rd e1) (substRD v rd e2)
substRD v rd (Join e1 e2) = Join (substRD v rd e1) (substRD v rd e2)
substRD v rd old
              | old==rd = v
              | otherwise = old

substAllins [] _ outs = outs
substAllins (v:vs) i outs = substAllins vs (i+1) (map (substRD v (AEin i)) outs)

substAllouts [] _ ins = ins
substAllouts (v:vs) i ins = substAllouts vs (i+1) (map (substRD v (AEout i)) ins)

eval (U s1 s2) = S.union (eval s1) (eval s2)
eval (Join s1 s2) = S.intersection (eval s1) (eval s2)
eval (Diff s1 s2) = S.difference (eval s1) (eval s2)
eval (Set s) = s

step (vIn, vOut) (ins, outs) = (map (Set . eval) $ res1,
                                map (Set . eval) $ res2)
                               where
                                 res1 = substAllouts vOut 1 ins
                                 res2 = substAllins vIn 1 outs

fixPoint (vIn, vOut) defs (prevIn, prevOut)
                | prevIn == vIn && prevOut == vOut = (vIn, vOut)
                | otherwise = fixPoint (step (vIn, vOut) defs) defs (vIn, vOut)

stepwise (vIn, vOut) res@(ins, outs) steps
                | steps==0 = (vIn, vOut)
                | otherwise = stepwise (newIn, newOut) res (steps-1)
                where (newIn, newOut) = step (vIn, vOut) res

solve eqs = fixPoint init defs ([],[])
          where
            defs = getDefs eqs
            -- actually, length (fst defs) == length (snd defs) :)
            init = (replicate (length $ fst defs) (_s [(v, e) | v <- vars, e <- aExpr]), 
                    replicate (length $ snd defs) (_s [(v, e) | v <- vars, e <- aExpr]))

-- usage
main = putStr $ ppResult $ solve eqs
