module Helper (parallelMap) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import System.IO.Unsafe (unsafePerformIO)

parallelMap :: (NFData b) => (a -> b) -> [a] -> [b]
parallelMap _ [] = []
parallelMap f xs = unsafePerformIO $ do
  mvars <- forM xs $ \x -> do
    mv <- newEmptyMVar
    _ <- forkIO $ evaluate (force (f x)) >>= putMVar mv
    return mv
  mapM takeMVar mvars