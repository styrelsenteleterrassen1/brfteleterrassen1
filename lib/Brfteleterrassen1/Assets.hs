module Brfteleterrassen1.Assets
  ( copyStaticAssets,
    copyDocuments,
  )
where

import Brfteleterrassen1.Models (Document (..), DocumentSection (..), DocumentsData (..))
import Control.Exception (throwIO)
import Data.Text qualified as T
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

contentDir :: FilePath
contentDir = "content"

webDir :: FilePath
webDir = "web"

-- | Copy all files from content/static/ to web/static/
copyStaticAssets :: IO ()
copyStaticAssets = do
  let srcDir = contentDir </> "static"
  let dstDir = webDir </> "static"

  -- Check if source directory exists
  exists <- doesDirectoryExist srcDir
  if not exists
    then putStrLn "Warning: content/static/ directory not found, skipping static assets"
    else do
      createDirectoryIfMissing True dstDir
      copyDirectoryRecursive srcDir dstDir

-- | Recursively copy directory contents
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  items <- listDirectory src
  mapM_ copyItem items
  where
    copyItem item = do
      let srcPath = src </> item
      let dstPath = dst </> item
      isDir <- doesDirectoryExist srcPath
      if isDir
        then copyDirectoryRecursive srcPath dstPath
        else do
          putStrLn $ "Copying: " <> srcPath <> " -> " <> dstPath
          copyFile srcPath dstPath

-- | Copy document PDFs from content/documents/ to web/documents/
copyDocuments :: DocumentsData -> IO ()
copyDocuments (DocumentsData {sections = docSections}) = do
  let destDir = webDir </> "documents"
  createDirectoryIfMissing True destDir

  mapM_
    (copyDoc destDir)
    (concatMap (\DocumentSection {documents = sectionDocs} -> sectionDocs) docSections)
  where
    copyDoc destDir (Document {file = fileName}) = do
      let srcPath = contentDir </> "documents" </> T.unpack fileName
      let dstPath = destDir </> T.unpack fileName

      exists <- doesFileExist srcPath
      if not exists
        then throwIO $ userError $ "Document file not found: " <> srcPath
        else do
          putStrLn $ "Copying document: " <> srcPath <> " -> " <> dstPath
          copyFile srcPath dstPath
