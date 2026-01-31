module Brfteleterrassen1.Content
  ( loadSiteData,
    validateDocumentFiles,
  )
where

import Brfteleterrassen1.Models
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | Base content directory - all input MUST come from here
contentDir :: FilePath
contentDir = "content"

-- | Load and decode a JSON file from content/
loadJSON :: (FromJSON a) => FilePath -> IO a
loadJSON relPath = do
  let fullPath = contentDir </> relPath
  exists <- doesFileExist fullPath
  if not exists
    then throwIO $ userError $ "Missing required file: " <> fullPath
    else do
      result <- eitherDecodeFileStrict fullPath
      case result of
        Left err -> throwIO $ userError $ "Failed to parse " <> fullPath <> ": " <> err
        Right val -> pure val

-- | Load all site data from content/
loadSiteData :: IO SiteData
loadSiteData = do
  config <- loadJSON "site.json"
  home <- loadJSON "pages/home.json"
  about <- loadJSON "pages/about.json"
  members <- loadJSON "pages/members.json"
  brokers <- loadJSON "pages/brokers.json"
  contact <- loadJSON "pages/contact.json"
  trivselregler <- loadJSON "pages/trivselregler.json"
  news <- loadJSON "news.json"
  docs <- loadJSON "documents/documents.json"
  pure
    SiteData
      { siteConfig = config,
        homePage = home,
        aboutPage = about,
        membersPage = members,
        brokersPage = brokers,
        contactPage = contact,
        trivselreglerPage = trivselregler,
        newsData = news,
        documentsData = docs
      }

-- | Validate that all referenced document files exist
validateDocumentFiles :: DocumentsData -> IO ()
validateDocumentFiles (DocumentsData {sections = docSections}) = do
  mapM_ checkFile (concatMap (\DocumentSection {documents = sectionDocs} -> sectionDocs) docSections)
  where
    checkFile (Document {file = fileName}) = do
      let filePath = contentDir </> "documents" </> T.unpack fileName
      exists <- doesFileExist filePath
      if not exists
        then
          throwIO $
            userError $
              "Document file not found: "
                <> filePath
                <> " (referenced in documents.json)"
        else pure ()
