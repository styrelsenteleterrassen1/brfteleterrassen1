module Brfteleterrassen1.Models where

import Data.Aeson
import Data.Text (Text)

-- | Site-level configuration
data SiteConfig = SiteConfig
  { name :: Text,
    description :: Text,
    headerImage :: Text
  }
  deriving (Show)

instance FromJSON SiteConfig where
  parseJSON = withObject "SiteConfig" $ \v ->
    SiteConfig
      <$> v .: "name"
      <*> v .: "description"
      <*> v .: "headerImage"

-- | A content section with heading and text
data Section = Section
  { heading :: Text,
    content :: Text
  }
  deriving (Show)

instance FromJSON Section where
  parseJSON = withObject "Section" $ \v ->
    Section
      <$> v .: "heading"
      <*> v .: "content"

-- | Home page data
data HomePage = HomePage
  { title :: Text,
    sections :: [Section]
  }
  deriving (Show)

instance FromJSON HomePage where
  parseJSON = withObject "HomePage" $ \v ->
    HomePage
      <$> v .: "title"
      <*> v .: "sections"

-- | A labeled field for displaying key-value information
data InfoField = InfoField
  { label :: Text,
    value :: Text
  }
  deriving (Show)

instance FromJSON InfoField where
  parseJSON = withObject "InfoField" $ \v ->
    InfoField
      <$> v .: "label"
      <*> v .: "value"

-- | About page data
data AboutPage = AboutPage
  { title :: Text,
    fields :: [InfoField]
  }
  deriving (Show)

instance FromJSON AboutPage where
  parseJSON = withObject "AboutPage" $ \v ->
    AboutPage
      <$> v .: "title"
      <*> v .: "fields"

-- | Members page data
data MembersPage = MembersPage
  { title :: Text,
    sections :: [Section]
  }
  deriving (Show)

instance FromJSON MembersPage where
  parseJSON = withObject "MembersPage" $ \v ->
    MembersPage
      <$> v .: "title"
      <*> v .: "sections"

-- | Brokers page data
data BrokersPage = BrokersPage
  { title :: Text,
    sections :: [Section]
  }
  deriving (Show)

instance FromJSON BrokersPage where
  parseJSON = withObject "BrokersPage" $ \v ->
    BrokersPage
      <$> v .: "title"
      <*> v .: "sections"

-- | A content section with heading and raw HTML content
data HtmlSection = HtmlSection
  { heading :: Text,
    contentHtml :: Text
  }
  deriving (Show)

instance FromJSON HtmlSection where
  parseJSON = withObject "HtmlSection" $ \v ->
    HtmlSection
      <$> v .: "heading"
      <*> v .: "contentHtml"

-- | Contact page data
data ContactPage = ContactPage
  { title :: Text,
    sections :: [HtmlSection]
  }
  deriving (Show)

instance FromJSON ContactPage where
  parseJSON = withObject "ContactPage" $ \v ->
    ContactPage
      <$> v .: "title"
      <*> v .: "sections"

-- | Document type
data DocumentType
  = Arsredovisning
  | Stadgar
  | Energideklaration
  | Ovrigt
  deriving (Show, Eq)

instance FromJSON DocumentType where
  parseJSON = withText "DocumentType" $ \case
    "årsredovisning" -> pure Arsredovisning
    "stadgar" -> pure Stadgar
    "energideklaration" -> pure Energideklaration
    "övrigt" -> pure Ovrigt
    other -> fail $ "Unknown document type: " <> show other

-- | Document metadata
data Document = Document
  { docType :: DocumentType,
    year :: Maybe Int,
    title :: Text,
    file :: Text,
    notes :: Maybe Text
  }
  deriving (Show)

instance FromJSON Document where
  parseJSON = withObject "Document" $ \v ->
    Document
      <$> v .: "type"
      <*> v .:? "year"
      <*> v .: "title"
      <*> v .: "file"
      <*> v .:? "notes"

-- | Documents collection
newtype DocumentsData = DocumentsData
  { documents :: [Document]
  }
  deriving (Show)

instance FromJSON DocumentsData where
  parseJSON = withObject "DocumentsData" $ \v ->
    DocumentsData
      <$> v .: "documents"

-- | All site data loaded from content/
data SiteData = SiteData
  { siteConfig :: SiteConfig,
    homePage :: HomePage,
    aboutPage :: AboutPage,
    membersPage :: MembersPage,
    brokersPage :: BrokersPage,
    contactPage :: ContactPage,
    documentsData :: DocumentsData
  }
  deriving (Show)
