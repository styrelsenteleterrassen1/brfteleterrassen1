module Brfteleterrassen1.Generator
  ( generate,
  )
where

import Brfteleterrassen1.Assets (copyDocuments, copyStaticAssets)
import Brfteleterrassen1.Content (loadSiteData, validateDocumentFiles)
import Brfteleterrassen1.Models (SiteData (..))
import Brfteleterrassen1.Pages
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

webDir :: FilePath
webDir = "web"

-- | Generate the entire static site
generate :: IO ()
generate = do
  putStrLn "=== BRF Teleterrassen 1 - Static Site Generator ==="
  putStrLn ""

  -- Load all site data from content/
  putStrLn "Loading site data from content/..."
  siteData <- loadSiteData
  putStrLn "Site data loaded successfully."
  putStrLn ""

  -- Validate that all referenced files exist
  putStrLn "Validating document files..."
  validateDocumentFiles siteData.documentsData
  putStrLn "All document files validated."
  putStrLn ""

  -- Create output directory
  putStrLn $ "Creating output directory: " <> webDir
  createDirectoryIfMissing True webDir
  putStrLn ""

  -- Generate HTML pages
  putStrLn "Generating HTML pages..."
  writeHtmlPage "index.html" $
    generateHomePage siteData.siteConfig siteData.homePage siteData.newsData
  writeHtmlPage "about.html" $
    generateAboutPage siteData.siteConfig siteData.aboutPage
  writeHtmlPage "members.html" $
    generateMembersPage siteData.siteConfig siteData.membersPage
  writeHtmlPage "brokers.html" $
    generateBrokersPage siteData.siteConfig siteData.brokersPage
  writeHtmlPage "contact.html" $
    generateContactPage siteData.siteConfig siteData.contactPage
  writeHtmlPage "trivselregler.html" $
    generateTrivselreglerPage siteData.siteConfig siteData.trivselreglerPage
  writeHtmlPage "documents.html" $
    generateDocumentsPage siteData.siteConfig siteData.documentsData
  putStrLn "HTML pages generated."
  putStrLn ""

  -- Copy static assets
  putStrLn "Copying static assets..."
  copyStaticAssets
  putStrLn "Static assets copied."
  putStrLn ""

  -- Copy documents
  putStrLn "Copying documents..."
  copyDocuments siteData.documentsData
  putStrLn "Documents copied."
  putStrLn ""

  putStrLn "=== Site generation complete! ==="
  putStrLn $ "Output directory: " <> webDir

-- | Write an HTML page to the web/ directory
writeHtmlPage :: FilePath -> Text -> IO ()
writeHtmlPage filename content = do
  let path = webDir </> filename
  putStrLn $ "Writing: " <> path
  TIO.writeFile path content
