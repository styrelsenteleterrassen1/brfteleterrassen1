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
import Brfteleterrassen1.Models
import Data.Char (isAlphaNum, toLower)
import Data.List (mapAccumL, sortOn)
import Data.Maybe (isJust, maybeToList)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

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
pageLayout :: SiteConfig -> Text -> Text -> Text -> Text
pageLayout config currentPage pageTitle content =
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
                <> pageNav currentPage
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
pageNav :: Text -> Text
pageNav current =
  nav_
    ( ul_
        ( li_ (navLink "index.html" "Hem" (current == "home"))
            <> li_ (navLink "about.html" "Om föreningen" (current == "about"))
            <> li_ (navLink "members.html" "För medlemmar" (current == "members"))
            <> li_ (navLink "trivselregler.html" "Trivselregler" (current == "trivselregler"))
            <> li_ (navLink "brokers.html" "För mäklare" (current == "brokers"))
            <> li_ (navLink "documents.html" "Dokument" (current == "documents"))
            <> li_ (navLink "contact.html" "Kontakt" (current == "contact"))
        )
    )
  where
    navLink href label isActive =
      let attrs = if isActive then [("href", href), ("class", "active")] else [("href", href)]
       in a attrs (escapeHtml label)

-- | Generate the page footer
pageFooter :: SiteConfig -> Text
pageFooter config =
  footer_ (p_ ("© " <> config.name))

-- | Generate the home page
generateHomePage :: SiteConfig -> HomePage -> NewsData -> Text
generateHomePage config page newsData =
  pageLayout config "home" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (.heading) page.sections))
      <> renderNewsFeed newsData
  where
    renderSection sec sectionId =
      section
        [("id", sectionId)]
        ( h3_ sec.heading
            <> p_ sec.content
        )

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
generateAboutPage :: SiteConfig -> AboutPage -> Text
generateAboutPage config page =
  pageLayout config "about" page.title $
    h2_ page.title
      <> section_
        (dl_ (T.concat (map renderField page.fields)))
  where
    renderField field =
      dt_ field.label
        <> dd_ field.value

-- | Generate the members page
generateMembersPage :: SiteConfig -> MembersPage -> Text
generateMembersPage config page =
  pageLayout config "members" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (.heading) page.sections))
  where
    renderSection sec sectionId =
      section
        [("id", sectionId)]
        ( h3_ sec.heading
            <> div_ sec.contentHtml
        )

-- | Generate the brokers page
generateBrokersPage :: SiteConfig -> BrokersPage -> Text
generateBrokersPage config page =
  pageLayout config "brokers" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (.heading) page.sections))
  where
    renderSection sec sectionId =
      section
        [("id", sectionId)]
        ( h3_ sec.heading
            <> p_ sec.content
        )

-- | Generate the contact page
generateContactPage :: SiteConfig -> ContactPage -> Text
generateContactPage config page =
  pageLayout config "contact" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (.heading) page.sections))
  where
    renderSection sec sectionId =
      section
        [("id", sectionId)]
        ( h3_ sec.heading
            <> div_ sec.contentHtml
        )

-- | Generate the trivselregler (house rules) page
generateTrivselreglerPage :: SiteConfig -> ContactPage -> Text
generateTrivselreglerPage config page =
  pageLayout config "trivselregler" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (.heading) page.sections))
  where
    renderSection sec sectionId =
      section
        [("id", sectionId)]
        ( h3_ sec.heading
            <> div_ sec.contentHtml
        )

-- | Generate the documents page
generateDocumentsPage :: SiteConfig -> DocumentsData -> Text
generateDocumentsPage config (DocumentsData docs) =
  pageLayout config "documents" "Dokument" $
    h2_ "Dokument"
      <> renderDocumentGroup "Årsredovisningar" Arsredovisning docs
      <> renderDocumentGroup "Stadgar" Stadgar docs
      <> renderDocumentGroup "Energideklarationer" Energideklaration docs
      <> renderDocumentGroup "Övrigt" Ovrigt docs
  where
    renderDocumentGroup groupTitle docType allDocs =
      let groupDocs = filter (\(Document {docType = dtype}) -> dtype == docType) allDocs
          sortedDocs = sortOn (Down . \(Document {year = y}) -> y) groupDocs
       in if null groupDocs
            then ""
            else
              section_
                ( h3_ groupTitle
                    <> ul_ (T.concat $ map renderDocument sortedDocs)
                )

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
