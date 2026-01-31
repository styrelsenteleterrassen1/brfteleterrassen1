module Brfteleterrassen1.HTML
  ( escapeHtml,
    attr,
    tag,
    tag_,
    doctype,
    html,
    html_,
    head_,
    body,
    body_,
    header,
    header_,
    nav,
    nav_,
    main_,
    section,
    section_,
    footer,
    footer_,
    div_,
    h1,
    h1_,
    h2,
    h2_,
    h3,
    h3_,
    h4,
    h4_,
    p,
    p_,
    a,
    ul,
    ul_,
    li,
    li_,
    dl,
    dl_,
    dt,
    dt_,
    dd,
    dd_,
    img,
    meta,
    link,
    title_,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape '\'' = "&#39;"
    escape c = T.singleton c

-- | Render an HTML attribute
attr :: Text -> Text -> Text
attr name value = " " <> name <> "=\"" <> escapeHtml value <> "\""

-- | Create an HTML tag with attributes and content
tag :: Text -> [(Text, Text)] -> Text -> Text
tag tagName attrs content =
  "<" <> tagName <> T.concat (map (uncurry attr) attrs) <> ">" <> content <> "</" <> tagName <> ">"

-- | Create a self-closing HTML tag with attributes
tag_ :: Text -> [(Text, Text)] -> Text
tag_ tagName attrs =
  "<" <> tagName <> T.concat (map (uncurry attr) attrs) <> ">"

-- | HTML doctype declaration
doctype :: Text
doctype = "<!DOCTYPE html>\n"

-- Concrete element helpers

html :: [(Text, Text)] -> Text -> Text
html = tag "html"

html_ :: Text -> Text
html_ = html []

head_ :: Text -> Text
head_ = tag "head" []

body :: [(Text, Text)] -> Text -> Text
body = tag "body"

body_ :: Text -> Text
body_ = body []

header :: [(Text, Text)] -> Text -> Text
header = tag "header"

header_ :: Text -> Text
header_ = header []

nav :: [(Text, Text)] -> Text -> Text
nav = tag "nav"

nav_ :: Text -> Text
nav_ = nav []

main_ :: Text -> Text
main_ = tag "main" []

section :: [(Text, Text)] -> Text -> Text
section = tag "section"

section_ :: Text -> Text
section_ = section []

footer :: [(Text, Text)] -> Text -> Text
footer = tag "footer"

footer_ :: Text -> Text
footer_ = footer []

div_ :: Text -> Text
div_ = tag "div" []

h1 :: [(Text, Text)] -> Text -> Text
h1 = tag "h1"

h1_ :: Text -> Text
h1_ = h1 []

h2 :: [(Text, Text)] -> Text -> Text
h2 = tag "h2"

h2_ :: Text -> Text
h2_ = h2 []

h3 :: [(Text, Text)] -> Text -> Text
h3 = tag "h3"

h3_ :: Text -> Text
h3_ = h3 []

h4 :: [(Text, Text)] -> Text -> Text
h4 = tag "h4"

h4_ :: Text -> Text
h4_ = h4 []

p :: [(Text, Text)] -> Text -> Text
p = tag "p"

p_ :: Text -> Text
p_ = p []

a :: [(Text, Text)] -> Text -> Text
a = tag "a"

ul :: [(Text, Text)] -> Text -> Text
ul = tag "ul"

ul_ :: Text -> Text
ul_ = ul []

li :: [(Text, Text)] -> Text -> Text
li = tag "li"

li_ :: Text -> Text
li_ = li []

dl :: [(Text, Text)] -> Text -> Text
dl = tag "dl"

dl_ :: Text -> Text
dl_ = dl []

dt :: [(Text, Text)] -> Text -> Text
dt = tag "dt"

dt_ :: Text -> Text
dt_ = dt []

dd :: [(Text, Text)] -> Text -> Text
dd = tag "dd"

dd_ :: Text -> Text
dd_ = dd []

img :: [(Text, Text)] -> Text
img = tag_ "img"

meta :: [(Text, Text)] -> Text
meta = tag_ "meta"

link :: [(Text, Text)] -> Text
link = tag_ "link"

title_ :: Text -> Text
title_ = tag "title" []
