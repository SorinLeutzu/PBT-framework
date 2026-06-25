module Helper (parallelMap) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import System.IO.Unsafe (unsafePerformIO)
import Config (numThreads)

parallelMap :: (NFData b) => (a -> b) -> [a] -> [b]
parallelMap _ [] = []
parallelMap f xs = unsafePerformIO $ do
  let chunks = chunksOf numThreads xs
  concat <$> mapM (mapChunk f) chunks

mapChunk :: (NFData b) => (a -> b) -> [a] -> IO [b]
mapChunk f xs = do
  mvars <- forM xs $ \x -> do
    mv <- newEmptyMVar
    _ <- forkIO $ evaluate (force (f x)) >>= putMVar mv
    return mv
  mapM takeMVar mvars

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t