module LiveVariables where

import qualified Data.Set as S
import Data.List
import Debug.Trace

data Expr = 
          LVin Int
          | LVout Int
          | Set (S.Set String)
          | Diff Expr Expr
          | U Expr Expr
          | Join Expr Expr 
          | Eqq Expr Expr
          deriving Eq

_s xs = Set $ S.fromList xs


instance Show Expr where
  show (LVin i) = "LVin(" ++ show i ++ ")"
  show (LVout i) = "LVout(" ++ show i ++ ")"
  show (Set s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"
  show (Diff e1 e2) = show e1 ++ " \\ " ++ show e2
  show (U e1 e2) = show e1 ++ " U " ++ show e2
  show (Join e1 e2) = show e1 ++ " Join " ++ show e2
  show (Eqq e1 e2) = show e1 ++ "=" ++ show e2

ppExpr :: [Expr] -> String
ppExpr = intercalate "\n" . map show

ppResult (rdins, rdouts) = "LVin: \n" ++ ppExpr rdins
                           ++ "\n\nLVout: \n" ++ ppExpr rdouts ++ "\n"

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

instance Show Aexpr where
  show (Mul e1 e2) = show e1 ++ "*" ++ show e2
  show (Add e1 e2) = show e1 ++ "+" ++ show e2
  show (Div e1 e2) = show e1 ++ "/" ++ show e2
  show (Minus e1 e2) = show e1 ++ "-" ++ show e2
  show (Lit i) = show i 
  show (Var v) = show v

v name = Var name
  
instance Num Aexpr where
  (*) = Mul
  (+) = Add
  (-) = Minus

vars = ["x", "y", "z"] -- "Var" set


varsOf (Mul e1 e2)  = varsOf e1 ++ varsOf e2
varsOf (Add e1 e2)  = varsOf e1 ++ varsOf e2
varsOf (Lit _)  = []
varsOf (Var x) = [x]


kill a =  _s a

-- use _s to make a set from list of pairs

eqs = [ LVin  1 .=. LVout 1 .\ kill ["x"],
        LVin  2 .=. LVout 2 .\ kill ["y"],
        LVin  3 .=. LVout 3 .\ kill ["x"],
        LVin  4 .=. LVout 4 `U` _s ["x", "y"],
        LVin  5 .=. LVout 5 .\ kill ["z"] `U` _s ["y"],
        LVin  6 .=. LVout 6 .\ kill ["z"] `U` _s ["y"],
        LVin  7 .=. LVout 7 .\ kill ["x"] `U` _s ["z"],
        
        LVout 1 .=. LVin 2,
        LVout 2 .=. LVin 3,
        LVout 3 .=. LVin 4,
        LVout 4 .=. LVin 5 `U` LVin 6,
        LVout 5 .=. LVin 7,
        LVout 6 .=. LVin 7,
        LVout 7 .=. _s []
      ]

-- TODO add ordering!!!
getDefs eqs = ([e1 | Eqq (LVin _) e1 <- eqs], [e2 | Eqq (LVout _) e2 <- eqs])

substRD v rd s@(Set _) = s
substRD v rd (Diff e1 e2) = Diff (substRD v rd e1) (substRD v rd e2)
substRD v rd (U e1 e2) = U (substRD v rd e1) (substRD v rd e2)
substRD v rd (Join e1 e2) = Join (substRD v rd e1) (substRD v rd e2)
substRD v rd old
              | old==rd = v
              | otherwise = old

substAllins [] _ outs = outs
substAllins (v:vs) i outs = substAllins vs (i+1) (map (substRD v (LVin i)) outs)

substAllouts [] _ ins = ins
substAllouts (v:vs) i ins = substAllouts vs (i+1) (map (substRD v (LVout i)) ins)

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
            init = (map Set $ replicate (length $ fst defs) S.empty, 
                    map Set $ replicate (length $ snd defs) S.empty)
-- usage
main = putStr $ ppResult $ solve eqs
