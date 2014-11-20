module ReachingDefs where

import qualified Data.Set as S
import Data.List

data Label = Labl Int
           | Undef
           deriving (Ord, Eq)

instance Show Label where
         show (Labl i) = show i
         show Undef = "?"

l n = Labl n

data RD = RDin Int
        | RDout Int
        deriving Eq

instance Show RD where
         show (RDin i) = "RDin(" ++ show i ++ ")"
         show (RDout i) = "RDout(" ++ show i ++ ")"

data Expr = Val RD
          | Set (S.Set (String, Label))
          | Diff Expr Expr
          | U Expr Expr
          | Eqq RD Expr
          deriving Eq

_v rd = Val rd
_s xs = Set $ S.fromList xs


instance Show Expr where
  show (Val s) = show s
  show (Set s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"
  show (Diff e1 e2) = show e1 ++ " \\ " ++ show e2
  show (U e1 e2) = show e1 ++ " U " ++ show e2
  show (Eqq e1 e2) = show e1 ++ "=" ++ show e2

ppExpr :: [Expr] -> String
ppExpr = intercalate "\n" . map show

ppResult (v1, v2) = ppExpr v1 ++ "\n" ++ ppExpr v2 ++ "\n"

e1 .\ e2 = Diff e1 e2

infix 1 .=.

i .=. s = Eqq i s

vars = ["x", "y", "z"]
labs = Undef : map l [1, 2, 3, 4, 5, 6]

-- use _s to make a set and _v to make a reference to DRin/out
eqs = [ RDin  1 .=. _s [(v, Undef) | v <- vars] ,
        RDin  2 .=. _v (RDout 1),
        RDin  3 .=. _v (RDout 2) `U` _v (RDout 5),
        RDin  4 .=. _v (RDout 3),
        RDin  5 .=. _v (RDout 4),
        RDin  6 .=. _v (RDout 3),
        RDout 1 .=. _v (RDin  1) .\ _s [("y", l) | l <- labs] `U` _s [("y",l 1)],
        RDout 2 .=. _v (RDin  2) .\ _s [("z", l) | l <- labs] `U` _s [("z",l 2)],
        RDout 3 .=. _v (RDin  3),
        RDout 4 .=. _v (RDin  4) .\ _s [("z", l) | l <- labs] `U` _s [("z",l 4)],
        RDout 5 .=. _v (RDin  5) .\ _s [("y", l) | l <- labs] `U` _s [("y",l 5)],
        RDout 6 .=. _v (RDin  6) .\ _s [("y", l) | l <- labs] `U` _s [("y",l 6)]
      ]

-- TODO add ordering!!!
getDefs eqs = ([e1 | Eqq (RDin _) e1 <- eqs], [e2 | Eqq (RDout _) e2 <- eqs])

--setRDin ds = 

substRD v rd old@(Val rd')
              | rd'==rd = v
              | otherwise = old
substRD v rd s@(Set _) = s
substRD v rd (Diff e1 e2) = Diff (substRD v rd e1) (substRD v rd e2)
substRD v rd (U e1 e2) = U (substRD v rd e1) (substRD v rd e2)

substAllins [] _ outs = outs
substAllins (v:vs) i outs = substAllins vs (i+1) (map (substRD v (RDin i)) outs)

substAllouts [] _ ins = ins
substAllouts (v:vs) i ins = substAllouts vs (i+1) (map (substRD v (RDout i)) ins)

eval (U s1 s2) = S.union (eval s1) (eval s2)
eval (Diff s1 s2) = S.difference (eval s1) (eval s2)
eval (Set s) = s

step (vIn, vOut) (ins, outs) = (map (Set . eval) $ substAllouts vIn 1 ins, map (Set . eval) $ substAllins vOut 1 outs)

step1 v eqs = map (Set . eval) $ substAllins v 1 outs
       where (ins, outs) = getDefs eqs

step2 v eqs = map (Set . eval) $ substAllouts v 1 ins
       where (ins, outs) = getDefs eqs

fixPoint (vOut, vIn) defs (prevIn, prevOut)
                | prevIn == vIn && prevOut == vOut = (vIn, vOut)
                | otherwise = fixPoint (newIn, newOut) defs (vIn, vOut)
                where (newOut, newIn) = step (vOut, vIn) defs

solve eqs = fixPoint init defs ([],[])
          where
            defs = getDefs eqs
            init = (replicate (length $ fst  defs) (Set S.empty),
                    replicate (length $ snd  defs) (Set S.empty))

fixPoint1 (vOut, vIn) res@(ins, outs) i
                | i==4 = (vIn, vOut)
                | otherwise = fixPoint1 (newOut, newIn) res (i+1)
                where (newOut, newIn) = step (vOut, vIn) res
-- usage
main = putStr $ ppResult $ solve eqs
