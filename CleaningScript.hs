import System.Directory
import System.FilePath
import Control.Monad

allowedExtensions :: [String]
allowedExtensions =
  [ ".hs"
  , ".txt"
  , ".png"
  , ".html"
  , ".md"
  , ".json"
  ]

allowedNames :: [String]
allowedNames =
  [ "CleaningScript.hs"
  ]

allowedFolders :: [String]
allowedFolders =
  [ ".git"
  , "baseDir"
  ]

cleanDirectory :: FilePath -> IO ()
cleanDirectory dir = do
  entries <- listDirectory dir
  forM_ entries $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then
        if name `elem` allowedFolders
          then putStrLn $ "  [SKIP FOLDER] " ++ path
          else do
            cleanDirectory path
            remaining <- listDirectory path
            when (null remaining) $ do
              removeDirectory path
              putStrLn $ "  [DELETE EMPTY DIR] " ++ path
      else do
        let ext = takeExtension name
        if ext `elem` allowedExtensions || name `elem` allowedNames
          then return ()
          else do
            removeFile path
            putStrLn $ "  [DELETE] " ++ path

main :: IO ()
main = do
  root <- getCurrentDirectory
  putStrLn $ "Cleaning project: " ++ root
  cleanDirectory root
  putStrLn "Done."