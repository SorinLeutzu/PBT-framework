module Helper (parallelMap) where

import GHC.Conc (par, pseq)

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f [] = []
parallelMap f (x:xs) = fx `par` fxs `pseq` (fx : fxs)
  where
    fx  = f x
    fxs = parallelMap f xs