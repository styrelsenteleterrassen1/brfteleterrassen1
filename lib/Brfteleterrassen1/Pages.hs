module Brfteleterrassen1.Pages
  ( SearchEntry (..),
    buildSearchEntries,
    generateHomePage,
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
import Data.Char (isAlphaNum, ord, toLower)
import Data.List (mapAccumL, sortOn)
import Data.Maybe (isJust, maybeToList)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data SearchEntry = SearchEntry
  { pageTitle :: Text,
    pageFile :: Text,
    sectionHeading :: Text,
    sectionId :: Text
  }
  deriving (Show)

sectionHeadingText :: Maybe Text -> Text
sectionHeadingText = maybe "" id

buildSearchEntries :: [(NavItem, Page)] -> [SearchEntry]
buildSearchEntries = concatMap (uncurry pageSearchEntries)

pageSearchEntries :: NavItem -> Page -> [SearchEntry]
pageSearchEntries navItem page =
  let sectionsWithIds = sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections
   in [ SearchEntry
          { pageTitle = navItem.title,
            pageFile = navItem.file,
            sectionHeading = headingText,
            sectionId = sectionId
          }
        | (sectionData, sectionId) <- sectionsWithIds,
          let headingText = T.strip (sectionHeadingText sectionData.heading),
          not (T.null headingText)
      ]

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
pageLayout :: SiteConfig -> [NavItem] -> [SearchEntry] -> Text -> Text -> Text -> Text
pageLayout config navItems searchEntries currentPage pageTitle content =
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
                <> pageNav navItems currentPage searchEntries
                <> main_ content
                <> pageFooter config
                <> searchScript searchEntries
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
pageNav :: [NavItem] -> Text -> [SearchEntry] -> Text
pageNav navItems current searchEntries =
  nav_
    ( tag
        "div"
        [("class", "nav-inner")]
        ( ul_
            (T.concat (map renderNavItem navItems))
            <> navSearch searchEntries
        )
    )
  where
    renderNavItem item =
      li_ (navLink item.file item.title (current == item.pageId))
    navLink href label isActive =
      let attrs = if isActive then [("href", href), ("class", "active")] else [("href", href)]
       in a attrs (escapeHtml label)
    navSearch entries
      | null entries = ""
      | otherwise =
          tag
            "div"
            [("class", "nav-search")]
            ( tag
                "label"
                [("class", "sr-only"), ("for", "site-search")]
                "Sok i rubriker"
                <> tag_
                  "input"
                  [ ("id", "site-search"),
                    ("type", "search"),
                    ("placeholder", "Sok i rubriker"),
                    ("autocomplete", "off"),
                    ("class", "nav-search-input"),
                    ("data-search-input", "true"),
                    ("aria-controls", "site-search-results"),
                    ("aria-expanded", "false"),
                    ("aria-autocomplete", "list")
                  ]
                <> ul
                  [ ("id", "site-search-results"),
                    ("class", "nav-search-results"),
                    ("data-search-results", "true"),
                    ("aria-live", "polite")
                  ]
                  ""
            )

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
generateHomePage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> NewsData -> Text
generateHomePage config navItems searchEntries page newsData =
  pageLayout config navItems searchEntries "home" page.title $
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
generateAboutPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateAboutPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "about" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the members page
generateMembersPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateMembersPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "members" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the brokers page
generateBrokersPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateBrokersPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "brokers" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the contact page
generateContactPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateContactPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "contact" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the trivselregler (house rules) page
generateTrivselreglerPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateTrivselreglerPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "trivselregler" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

-- | Generate the documents page
generateDocumentsPage :: SiteConfig -> [NavItem] -> [SearchEntry] -> Page -> Text
generateDocumentsPage config navItems searchEntries page =
  pageLayout config navItems searchEntries "documents" page.title $
    h2_ page.title
      <> T.concat (map (uncurry renderSection) (sortedSectionsWithIds (sectionHeadingText . (.heading)) page.sections))

searchScript :: [SearchEntry] -> Text
searchScript entries
  | null entries = ""
  | otherwise =
      tag
        "script"
        []
        ( T.intercalate
            "\n"
            [ "(function() {",
              "  const searchIndex = " <> searchEntriesJson entries <> ";",
              "  const input = document.querySelector('[data-search-input]');",
              "  const results = document.querySelector('[data-search-results]');",
              "  if (!input || !results || searchIndex.length === 0) {",
              "    return;",
              "  }",
              "  const normalize = (value) => value",
              "    .toLocaleLowerCase('sv')",
              "    .normalize('NFKD')",
              "    .replace(/[\\u0300-\\u036f]/g, '');",
              "  const fuzzyScore = (query, text) => {",
              "    let score = 0;",
              "    let lastIndex = -1;",
              "    for (const ch of query) {",
              "      const idx = text.indexOf(ch, lastIndex + 1);",
              "      if (idx === -1) return null;",
              "      score += idx;",
              "      lastIndex = idx;",
              "    }",
              "    return score;",
              "  };",
              "  const renderResults = (query) => {",
              "    const trimmed = query.trim();",
              "    results.replaceChildren();",
              "    if (!trimmed) {",
              "      results.classList.remove('is-visible');",
              "      input.setAttribute('aria-expanded', 'false');",
              "      return;",
              "    }",
              "    const normalizedQuery = normalize(trimmed);",
              "    const matches = [];",
              "    for (const entry of searchIndex) {",
              "      const score = fuzzyScore(normalizedQuery, normalize(entry.sectionHeading));",
              "      if (score !== null) {",
              "        matches.push({ entry, score });",
              "      }",
              "    }",
              "    matches.sort((a, b) => (a.score - b.score) || a.entry.sectionHeading.localeCompare(b.entry.sectionHeading, 'sv'));",
              "    const limit = 8;",
              "    const toShow = matches.slice(0, limit);",
              "    if (toShow.length === 0) {",
              "      const emptyItem = document.createElement('li');",
              "      emptyItem.className = 'nav-search-empty';",
              "      emptyItem.textContent = 'Inga traffar';",
              "      results.appendChild(emptyItem);",
              "    } else {",
              "      for (const match of toShow) {",
              "        const item = document.createElement('li');",
              "        const link = document.createElement('a');",
              "        link.href = match.entry.pageFile + '#' + match.entry.sectionId;",
              "        link.textContent = match.entry.pageTitle + ' - ' + match.entry.sectionHeading;",
              "        item.appendChild(link);",
              "        results.appendChild(item);",
              "      }",
              "    }",
              "    results.classList.add('is-visible');",
              "    input.setAttribute('aria-expanded', 'true');",
              "  };",
              "  input.addEventListener('input', (event) => {",
              "    renderResults(event.target.value);",
              "  });",
              "  input.addEventListener('focus', (event) => {",
              "    renderResults(event.target.value);",
              "  });",
              "  document.addEventListener('click', (event) => {",
              "    if (!event.target.closest('.nav-search')) {",
              "      results.classList.remove('is-visible');",
              "      input.setAttribute('aria-expanded', 'false');",
              "    }",
              "  });",
              "})();"
            ]
        )

searchEntriesJson :: [SearchEntry] -> Text
searchEntriesJson entries =
  "[" <> T.intercalate "," (map renderSearchEntry entries) <> "]"

renderSearchEntry :: SearchEntry -> Text
renderSearchEntry entry =
  "{"
    <> "\"pageTitle\":"
    <> jsonString entry.pageTitle
    <> ",\"pageFile\":"
    <> jsonString entry.pageFile
    <> ",\"sectionHeading\":"
    <> jsonString entry.sectionHeading
    <> ",\"sectionId\":"
    <> jsonString entry.sectionId
    <> "}"

jsonString :: Text -> Text
jsonString value =
  "\"" <> T.concatMap escapeJsonChar value <> "\""

escapeJsonChar :: Char -> Text
escapeJsonChar c = case c of
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  _ | ord c < 0x20 -> "\\u" <> T.pack (pad4 (showHex (ord c) ""))
  _ -> T.singleton c

pad4 :: String -> String
pad4 s = replicate (4 - length s) '0' <> s
