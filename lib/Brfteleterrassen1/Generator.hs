module Brfteleterrassen1.Generator
  ( generate,
  )
where

import Brfteleterrassen1.Assets (copyDocuments, copyStaticAssets)
import Brfteleterrassen1.Content (loadSiteData, validateDocumentFiles)
import Brfteleterrassen1.Models (NavItem (..), Page (..), SiteData (..))
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
  validateDocumentFiles siteData.documentsPage
  putStrLn "All document files validated."
  putStrLn ""

  -- Create output directory
  putStrLn $ "Creating output directory: " <> webDir
  createDirectoryIfMissing True webDir
  putStrLn ""

  -- Build navigation items from page titles
  let navItemsWithPages =
        [ (NavItem "index.html" "home" siteData.homePage.title, siteData.homePage),
          (NavItem "about.html" "about" siteData.aboutPage.title, siteData.aboutPage),
          (NavItem "members.html" "members" siteData.membersPage.title, siteData.membersPage),
          (NavItem "trivselregler.html" "trivselregler" siteData.trivselreglerPage.title, siteData.trivselreglerPage),
          (NavItem "brokers.html" "brokers" siteData.brokersPage.title, siteData.brokersPage),
          (NavItem "documents.html" "documents" siteData.documentsPage.title, siteData.documentsPage),
          (NavItem "contact.html" "contact" siteData.contactPage.title, siteData.contactPage)
        ]
      navItems = map fst navItemsWithPages
      searchEntries = buildSearchEntries navItemsWithPages

  -- Generate HTML pages
  putStrLn "Generating HTML pages..."
  writeHtmlPage "index.html" $
    generateHomePage siteData.siteConfig navItems searchEntries siteData.homePage siteData.newsData
  writeHtmlPage "about.html" $
    generateAboutPage siteData.siteConfig navItems searchEntries siteData.aboutPage
  writeHtmlPage "members.html" $
    generateMembersPage siteData.siteConfig navItems searchEntries siteData.membersPage
  writeHtmlPage "brokers.html" $
    generateBrokersPage siteData.siteConfig navItems searchEntries siteData.brokersPage
  writeHtmlPage "contact.html" $
    generateContactPage siteData.siteConfig navItems searchEntries siteData.contactPage
  writeHtmlPage "trivselregler.html" $
    generateTrivselreglerPage siteData.siteConfig navItems searchEntries siteData.trivselreglerPage
  writeHtmlPage "documents.html" $
    generateDocumentsPage siteData.siteConfig navItems searchEntries siteData.documentsPage
  putStrLn "HTML pages generated."
  putStrLn ""

  -- Copy static assets
  putStrLn "Copying static assets..."
  copyStaticAssets
  putStrLn "Static assets copied."
  putStrLn ""

  -- Copy documents
  putStrLn "Copying documents..."
  copyDocuments siteData.documentsPage
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
