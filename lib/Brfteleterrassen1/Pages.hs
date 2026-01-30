module Brfteleterrassen1.Pages
  ( generateHomePage,
    generateAboutPage,
    generateMembersPage,
    generateBrokersPage,
    generateContactPage,
    generateDocumentsPage,
  )
where

import Brfteleterrassen1.HTML
import Brfteleterrassen1.Models
import Data.List (sortOn)
import Data.Maybe (isJust, maybeToList)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | Generate the HTML page layout
pageLayout :: SiteConfig -> Text -> Text -> Text -> Text
pageLayout config currentPage pageTitle content =
  doctype
    <> html_
      ( head_
          ( title_ (config.name <> " - " <> pageTitle)
              <> meta [("charset", "utf-8")]
              <> meta [("name", "viewport"), ("content", "width=device-width, initial-scale=1")]
              <> link [("rel", "stylesheet"), ("href", "/static/style.css")]
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
        [ ("src", "/static/" <> config.headerImage),
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
        ( li_ (navLink "/" "Hem" (current == "home"))
            <> li_ (navLink "/about.html" "Om föreningen" (current == "about"))
            <> li_ (navLink "/members.html" "För medlemmar" (current == "members"))
            <> li_ (navLink "/brokers.html" "För mäklare" (current == "brokers"))
            <> li_ (navLink "/documents.html" "Dokument" (current == "documents"))
            <> li_ (navLink "/contact.html" "Kontakt" (current == "contact"))
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
generateHomePage :: SiteConfig -> HomePage -> Text
generateHomePage config page =
  pageLayout config "home" page.title $
    h2_ page.title
      <> T.concat (map renderSection page.sections)
  where
    renderSection sec =
      section_
        ( h3_ sec.heading
            <> p_ sec.content
        )

-- | Generate the about page
generateAboutPage :: SiteConfig -> AboutPage -> Text
generateAboutPage config page =
  pageLayout config "about" page.title $
    h2_ page.title
      <> section_
        ( dl_ (T.concat (map renderField page.fields))
        )
  where
    renderField field =
      dt_ field.label
        <> dd_ field.value

-- | Generate the members page
generateMembersPage :: SiteConfig -> MembersPage -> Text
generateMembersPage config page =
  pageLayout config "members" page.title $
    h2_ page.title
      <> T.concat (map renderSection page.sections)
  where
    renderSection sec =
      section_
        ( h3_ sec.heading
            <> p_ sec.content
        )

-- | Generate the brokers page
generateBrokersPage :: SiteConfig -> BrokersPage -> Text
generateBrokersPage config page =
  pageLayout config "brokers" page.title $
    h2_ page.title
      <> T.concat (map renderSection page.sections)
  where
    renderSection sec =
      section_
        ( h3_ sec.heading
            <> p_ sec.content
        )

-- | Generate the contact page
generateContactPage :: SiteConfig -> ContactPage -> Text
generateContactPage config page =
  pageLayout config "contact" page.title $
    h2_ page.title
      <> T.concat (map renderSection page.sections)
  where
    renderSection sec =
      section_
        ( h3_ sec.heading
            <> p_ sec.contentHtml
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
        ( a [("href", "/documents/" <> docFile)] docTitle
            <> ( if isJust docYear
                   then " (" <> T.pack (show $ maybe 0 id docYear) <> ")"
                   else ""
               )
            <> T.concat (maybeToList (fmap renderNotes docNotes))
        )

    renderNotes notes = " - " <> notes
