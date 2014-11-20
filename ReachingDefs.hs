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

{-
data RD = RDin Int
        | RDout Int
        deriving Eq

instance Show RD where
         show (RDin i) = "RDin(" ++ show i ++ ")"
         show (RDout i) = "RDout(" ++ show i ++ ")"
-}
data Expr = 
          RDin Int
          | RDout Int
          | Set (S.Set (String, Label))
          | Diff Expr Expr
          | U Expr Expr
          | Eqq Expr Expr
          deriving Eq

-- _v rd = Val rd
_s xs = Set $ S.fromList xs


instance Show Expr where
  show (RDin i) = "RDin(" ++ show i ++ ")"
  show (RDout i) = "RDout(" ++ show i ++ ")"
  show (Set s) = "{" ++ intercalate "," (map show $ S.toList s) ++ "}"
  show (Diff e1 e2) = show e1 ++ " \\ " ++ show e2
  show (U e1 e2) = show e1 ++ " U " ++ show e2
  show (Eqq e1 e2) = show e1 ++ "=" ++ show e2

ppExpr :: [Expr] -> String
ppExpr = intercalate "\n" . map show

ppResult (rdins, rdouts) = "RDin: \n" ++ ppExpr rdins
                           ++ "\n\nDRout: \n" ++ ppExpr rdouts ++ "\n"

e1 .\ e2 = Diff e1 e2

infix 1 .=.

i .=. s = Eqq i s

vars = ["x", "y", "z"] -- "Var" set 
labs = Undef : map l [1, 2, 3, 4, 5, 6] -- "Lab?"

-- use _s to make a set from list of pairs
eqs = [ RDin  1 .=. _s [(v, Undef) | v <- vars] ,
        RDin  2 .=. RDout 1,
        RDin  3 .=. RDout 2 `U` (RDout 5),
        RDin  4 .=. RDout 3,
        RDin  5 .=. RDout 4,
        RDin  6 .=. RDout 3,
        
        RDout 1 .=. RDin  1 .\ _s [("y", l) | l <- labs] `U` _s [("y",l 1)],
        RDout 2 .=. RDin  2 .\ _s [("z", l) | l <- labs] `U` _s [("z",l 2)],
        RDout 3 .=. RDin  3,
        RDout 4 .=. RDin  4 .\ _s [("z", l) | l <- labs] `U` _s [("z",l 4)],
        RDout 5 .=. RDin  5 .\ _s [("y", l) | l <- labs] `U` _s [("y",l 5)],
        RDout 6 .=. RDin  6 .\ _s [("y", l) | l <- labs] `U` _s [("y",l 6)]
      ]

-- TODO add ordering!!!
getDefs eqs = ([e1 | Eqq (RDin _) e1 <- eqs], [e2 | Eqq (RDout _) e2 <- eqs])

--setRDin ds = 

substRD v rd s@(Set _) = s
substRD v rd (Diff e1 e2) = Diff (substRD v rd e1) (substRD v rd e2)
substRD v rd (U e1 e2) = U (substRD v rd e1) (substRD v rd e2)
substRD v rd old
              | old==rd = v
              | otherwise = old

substAllins [] _ outs = outs
substAllins (v:vs) i outs = substAllins vs (i+1) (map (substRD v (RDin i)) outs)

substAllouts [] _ ins = ins
substAllouts (v:vs) i ins = substAllouts vs (i+1) (map (substRD v (RDout i)) ins)

eval (U s1 s2) = S.union (eval s1) (eval s2)
eval (Diff s1 s2) = S.difference (eval s1) (eval s2)
eval (Set s) = s

step (vIn, vOut) (ins, outs) = (map (Set . eval) $ substAllouts vOut 1 ins,
                                map (Set . eval) $ substAllins vIn 1 outs)

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
            -- acually length (fst defs) == length (snd defs) :)
            init = (replicate (length $ fst defs) (Set S.empty), 
                    replicate (length $ snd defs) (Set S.empty))


-- solution from slides
modelSolution = ([ -- RDin
  [("x", Undef), ("y", Undef), ("z", Undef)],
  [("x", Undef), ("y", l 1), ("z", Undef)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 2), ("z", l 4)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 2), ("z", l 4)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 4)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 2), ("z", l 4)]
  ],
                 [ -- RDout
  [("x", Undef), ("y", l 1), ("z", Undef)],
  [("x", Undef), ("y", l 1), ("z", l 2)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 2), ("z", l 4)],
  [("x", Undef), ("y", l 1), ("y", l 5), ("z", l 4)],
  [("x", Undef), ("y", l 5), ("z", l 4)],
  [("x", Undef), ("y", l 6), ("z", l 2), ("z", l 4)]
  ]
  )

test = modelData == computedData
     where
       (ins, outs) = modelSolution
       modelData = (map (Set . S.fromList) ins, map (Set . S.fromList) outs)
       computedData = solve eqs

-- stabilisation after 10 steps
test1 = modelData == computedData
     where
       (ins, outs) = modelSolution
       modelData = (map (Set . S.fromList) ins, map (Set . S.fromList) outs)
       computedData = stepwise init defs 10
       init = (replicate (length $ fst defs) (Set S.empty), 
               replicate (length $ snd defs) (Set S.empty))
       defs = getDefs eqs

-- usage
main = putStr $ ppResult $ solve eqs
