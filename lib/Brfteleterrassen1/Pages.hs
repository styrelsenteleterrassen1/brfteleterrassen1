module Brfteleterrassen1.Pages
  ( generateHomePage,
    generateAboutPage,
    generateMembersPage,
    generateBrokersPage,
    generateContactPage,
    generateTrivselreglerPage,
    generateDocumentsPage,
  )
where

import Brfteleterrassen1.HTML
import Brfteleterrassen1.Models (Document (..), InfoField (..), NavItem (..), NewsData (..), NewsItem (..), Page (..), Section (..), SectionContent (..), SiteConfig (..))
import Data.Char (isAlphaNum, toLower)
import Data.List (mapAccumL, sortOn)
import Data.Maybe (isJust, maybeToList)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

sectionHeadingText :: Maybe Text -> Text
sectionHeadingText = maybe "" id

sortedSectionsWithIds :: (entry -> Text) -> [entry] -> [(entry, Text)]
sortedSectionsWithIds getHeading sections =
  let sortedSections = sortOn (T.toCaseFold . getHeading) sections
      sectionIds = assignSectionIds (map getHeading sortedSections)
   in zip sortedSections sectionIds

assignSectionIds :: [Text] -> [Text]
assignSectionIds headings =
  snd $ mapAccumL assignId [] headings
  where
    assignId counts heading =
      let base = sectionIdBase heading
          count = countFor base counts
          suffix = if count == 0 then "" else "-" <> T.pack (show (count + 1))
          sectionId = base <> suffix
       in (updateCount base (count + 1) counts, sectionId)

    countFor key counts = maybe 0 id (lookup key counts)
    updateCount key newCount [] = [(key, newCount)]
    updateCount key newCount ((entryKey, entryCount) : rest)
      | entryKey == key = (entryKey, newCount) : rest
      | otherwise = (entryKey, entryCount) : updateCount key newCount rest

sectionIdBase :: Text -> Text
sectionIdBase heading =
  let base = slugify heading
   in if T.null base then "section" else base

slugify :: Text -> Text
slugify =
  trimDashes . collapseDashes . T.map replaceChar . T.toLower
  where
    replaceChar c =
      if isAlphaNum c
        then toLower c
        else '-'
    collapseDashes = T.pack . go False . T.unpack
    go _ [] = []
    go prevDash (x : xs)
      | x == '-' = if prevDash then go True xs else x : go True xs
      | otherwise = x : go False xs
    trimDashes = T.dropAround (== '-')

-- | Generate the HTML page layout
pageLayout :: SiteConfig -> [NavItem] -> Text -> Text -> Text -> Text
pageLayout config navItems currentPage pageTitle content =
  doctype
    <> html_
      ( head_
          ( title_ (config.name <> " - " <> pageTitle)
              <> meta [("charset", "utf-8")]
              <> meta [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
              <> link [("rel", "stylesheet"), ("href", "static/style.css")]
          )
          <> body_
            ( pageHeader config
                <> pageNav navItems currentPage
                <> main_ content
                <> pageFooter config
            )
      )

-- | Generate the page header with hero image
pageHeader :: SiteConfig -> Text
pageHeader config =
  header
    [("class", "site-header")]
    ( img
        [ ("src", "static/" <> config.headerImage),
          ("alt", config.name),
          ("class", "header-image")
        ]
        <> h1_ config.name
    )

-- | Generate the navigation menu
pageNav :: [NavItem] -> Text -> Text
pageNav navItems current =
  nav_
    ( ul_
        (T.concat (map renderNavItem navItems))
    )
  where
    renderNavItem item =
      li_ (navLink item.file item.title (current == item.pageId))
    navLink href label isActive =
      let attrs = if isActive then [("href", href), ("class", "active")] else [("href", href)]
       in a attrs (escapeHtml label)

-- | Generate the page footer
pageFooter :: SiteConfig -> Text
pageFooter config =
  footer_ (p_ ("© " <> config.name))

renderSectionHeading :: Text -> Text
renderSectionHeading heading =
  if T.null heading then "" else h3_ heading

renderFields :: [InfoField] -> Text
renderFields fields =
  dl_ (T.concat (map renderField fields))
  where
    renderField field =
      dt_ field.label
        <> dd_ field.value

renderDocuments :: [Document] -> Text
renderDocuments docs =
  let sortedDocs = sortOn (Down . (\doc -> doc.year)) docs
   in ul_ (T.concat $ map renderDocument sortedDocs)
  where
    renderDocument (Document {title = docTitle, file = docFile, year = docYear, notes = docNotes}) =
      li_
        ( a [("href", "documents/" <> docFile)] docTitle
            <> ( if isJust docYear
                   then " (" <> T.pack (show $ maybe 0 id docYear) <> ")"
                   else ""
               )
            <> T.concat (maybeToList (fmap renderNotes docNotes))
        )

    renderNotes notes = " - " <> notes

renderSection :: Section -> Text -> Text
renderSection (Section {heading = sectionHeading, content = sectionContent}) sectionId =
  let headingText = sectionHeadingText sectionHeading
      headingMarkup = renderSectionHeading headingText
   in case sectionContent of
        TextContent textContent ->
          section
            [("id", sectionId)]
            ( headingMarkup
                <> p_ textContent
            )
        HtmlContent htmlContent ->
          section
            [("id", sectionId)]
            ( headingMarkup
                <> div_ htmlContent
            )
        FieldsContent fields ->
          section
            [("id", sectionId)]
            ( headingMarkup
                <> renderFields fields
            )
        DocumentsContent docs ->
          if null docs
            then ""
            else
              section
                [("id", sectionId)]
                ( headingMarkup
                    <> renderDocuments docs
                )

-- | Generate the home page
generateHomePage :: SiteConfig -> [NavItem] -> Page -> NewsData -> Text
generateHomePage config navItems page newsData =
  pageLayout config navItems "home" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))
      <> renderNewsFeed newsData
  where
    renderNewsFeed (NewsData items) =
      if null items
        then ""
        else
          section [("class", "news-feed")] $
            h3_ "Nyhetsflöde"
              <> T.concat (map renderNewsItem items)

    renderNewsItem (NewsItem {title = itemTitle, date = itemDate, text = itemText, poster = itemPoster}) =
      section [("class", "news-item")] $
        h4_ itemTitle
          <> p [("class", "news-meta")] ("Datum: " <> itemDate <> " • Publicerat av " <> itemPoster)
          <> p_ itemText

-- | Generate the about page
generateAboutPage :: SiteConfig -> [NavItem] -> Page -> Text
generateAboutPage config navItems page =
  pageLayout config navItems "about" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the members page
generateMembersPage :: SiteConfig -> [NavItem] -> Page -> Text
generateMembersPage config navItems page =
  pageLayout config navItems "members" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the brokers page
generateBrokersPage :: SiteConfig -> [NavItem] -> Page -> Text
generateBrokersPage config navItems page =
  pageLayout config navItems "brokers" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the contact page
generateContactPage :: SiteConfig -> [NavItem] -> Page -> Text
generateContactPage config navItems page =
  pageLayout config navItems "contact" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the trivselregler (house rules) page
generateTrivselreglerPage :: SiteConfig -> [NavItem] -> Page -> Text
generateTrivselreglerPage config navItems page =
  pageLayout config navItems "trivselregler" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the documents page
generateDocumentsPage :: SiteConfig -> [NavItem] -> Page -> Text
generateDocumentsPage config navItems page =
  pageLayout config navItems "documents" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))
