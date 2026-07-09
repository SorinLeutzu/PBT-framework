module CleaningScript where

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
  , ".tex"
  ]

allowedNames :: [String]
allowedNames =
  [ "CleaningScript.hs"
  ]

allowedFolders :: [String]
allowedFolders =
  [ ".git"
  , "baseDir"
  , "ceva"
  , "studentVersion"
  , "diagrams"
  , "figures"
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


copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirectory srcPath dstPath
      else copyFile srcPath dstPath

studentExcludedFolders :: [String]
studentExcludedFolders =
  [ "benchmark"
  , "moduleTesting"
  ]

generateStudentVersion :: FilePath -> IO ()
generateStudentVersion root = do
  let studentDir = root </> "studentVersion"
  studentExists <- doesDirectoryExist studentDir
  when studentExists $ do
    removeDirectoryRecursive studentDir
    putStrLn $ "  [REMOVED OLD] " ++ studentDir
  createDirectoryIfMissing True studentDir
  entries <- listDirectory root
  forM_ entries $ \name -> do
    let srcPath = root </> name
    isDir <- doesDirectoryExist srcPath
    if name == "studentVersion" || name `elem` allowedFolders
      then putStrLn $ "  [SKIP] " ++ name
      else if isDir
        then if name `elem` studentExcludedFolders
          then putStrLn $ "  [EXCLUDE] " ++ name
          else do
            copyDirectory srcPath (studentDir </> name)
            putStrLn $ "  [COPY DIR] " ++ name
        else do
          copyFile srcPath (studentDir </> name)
          putStrLn $ "  [COPY FILE] " ++ name
  putStrLn $ "Student version generated at: " ++ studentDir

main :: IO ()
main = do
  root <- getCurrentDirectory
  putStrLn $ "Cleaning project: " ++ root
  cleanDirectory root
  putStrLn ""
  putStrLn "Generating student version..."
  generateStudentVersion root
  putStrLn "Done."