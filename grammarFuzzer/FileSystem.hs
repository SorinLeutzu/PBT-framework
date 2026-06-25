module GrammarFuzzer.FileSystem where

import GrammarFuzzer.GrammarDefinitions

import System.Directory (createDirectoryIfMissing, canonicalizePath)
import System.IO (writeFile)
import System.FilePath ((</>))


-- creates a folder
createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

-- creates a text file
createTextFile :: FilePath -> String -> IO ()
createTextFile = writeFile

baseDirPath :: FilePath
baseDirPath = "baseDir"


walkFileSystemTreeWrapper :: ParseTree -> IO ()
walkFileSystemTreeWrapper tree = do
  createFolder baseDirPath
  resolved <- canonicalizePath baseDirPath
  putStrLn $ "Building file system under: " ++ resolved
  walkFileSystemTree baseDirPath tree

walkFileSystemTree :: FilePath -> ParseTree -> IO ()
walkFileSystemTree currentPath (Tree parentSym children) = do
  let params = words (show parentSym)
  case params of
    ("D" : name : _) -> do
      let newPath = currentPath </> name
      createFolder newPath
      mapM_ (walkFileSystemTree newPath) children
    _ ->
      mapM_ (walkFileSystemTree currentPath) children

walkFileSystemTree currentPath (Leaf value _) = do
  let params = words (show value)
  case params of
    ("f" : name : rest) ->
      createTextFile (currentPath </> name) (unwords rest)
    ("D" : name : _) ->
      createFolder (currentPath </> name)
    ("E" : name : _) ->
      createFolder (currentPath </> name)
    _ -> return ()