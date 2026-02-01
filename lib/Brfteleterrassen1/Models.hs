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

data SectionKind
  = TextKind
  | HtmlKind
  | FieldsKind
  | DocumentsKind
  deriving (Show, Eq)

instance FromJSON SectionKind where
  parseJSON = withText "SectionKind" $ \case
    "text" -> pure TextKind
    "html" -> pure HtmlKind
    "fields" -> pure FieldsKind
    "documents" -> pure DocumentsKind
    other -> fail $ "Unknown section kind: " <> show other

-- | News item data
data NewsItem = NewsItem
  { title :: Text,
    date :: Text,
    text :: Text,
    poster :: Text
  }
  deriving (Show)

instance FromJSON NewsItem where
  parseJSON = withObject "NewsItem" $ \v ->
    NewsItem
      <$> v .: "title"
      <*> v .: "date"
      <*> v .: "text"
      <*> v .: "poster"

-- | News collection
newtype NewsData = NewsData
  { news :: [NewsItem]
  }
  deriving (Show)

instance FromJSON NewsData where
  parseJSON = withObject "NewsData" $ \v ->
    NewsData
      <$> v .: "news"

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

data SectionContent
  = TextContent Text
  | HtmlContent Text
  | FieldsContent [InfoField]
  | DocumentsContent [Document]
  deriving (Show)

data Section = Section
  { heading :: Maybe Text,
    content :: SectionContent
  }
  deriving (Show)

instance FromJSON Section where
  parseJSON = withObject "Section" $ \v -> do
    heading <- v .:? "heading"
    kind <- v .: "kind"
    content <- case kind of
      TextKind -> TextContent <$> v .: "content"
      HtmlKind -> HtmlContent <$> v .: "content"
      FieldsKind -> FieldsContent <$> v .: "fields"
      DocumentsKind -> DocumentsContent <$> v .: "documents"
    pure Section {heading, content}

data Page = Page
  { title :: Text,
    sections :: [Section]
  }
  deriving (Show)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \v ->
    Page
      <$> v .: "title"
      <*> v .: "sections"

-- | Navigation item for menu
data NavItem = NavItem
  { file :: Text,
    pageId :: Text,
    title :: Text
  }
  deriving (Show)

-- | All site data loaded from content/
data SiteData = SiteData
  { siteConfig :: SiteConfig,
    homePage :: Page,
    aboutPage :: Page,
    membersPage :: Page,
    brokersPage :: Page,
    contactPage :: Page,
    trivselreglerPage :: Page,
    newsData :: NewsData,
    documentsPage :: Page
  }
  deriving (Show)
