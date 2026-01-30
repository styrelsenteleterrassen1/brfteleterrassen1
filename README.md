# BRF Teleterrassen 1 - Static Site Generator

A static website generator for Swedish housing associations (BRF), built with Haskell.

## Overview

This generator reads content from JSON files in the `content/` directory and produces a complete static website in the `web/` directory. The generated site includes:

- Home page
- About the association
- Information for members
- Information for real estate brokers
- Document library (grouped by type)
- Contact information

## Prerequisites

- Nix with flakes enabled
- All development happens inside the Nix development shell

## Quick Start

```bash
# Enter the Nix development shell
nix develop

# Build and run the generator
nix run .

# Or use cabal directly (from within nix develop)
cabal run brfteleterrassen1
```

The generated site will be in the `web/` directory.

## Content Structure

All content must be placed under the `content/` directory:

```
content/
├── site.json                # Global site configuration
├── pages/
│   ├── home.json           # Home page content
│   ├── about.json          # About page content
│   ├── members.json        # Members page content
│   ├── brokers.json        # Brokers page content
│   └── contact.json        # Contact page content
├── documents/
│   ├── documents.json      # Document metadata
│   └── *.pdf              # PDF files
└── static/
    ├── style.css          # Stylesheet
    ├── header.png         # Header image
    └── images/            # Optional images
```

## Output Structure

The generator produces the following in `web/`:

```
web/
├── index.html             # Home page
├── about.html             # About page
├── members.html           # Members page
├── brokers.html           # Brokers page
├── contact.html           # Contact page
├── documents.html         # Documents listing
├── static/                # Copied from content/static/
│   ├── style.css
│   ├── header.png
│   └── images/
└── documents/             # Copied from content/documents/
    └── *.pdf
```

## Editing Content

### Site Configuration (site.json)

```json
{
  "name": "BRF Teleterrassen 1",
  "description": "Bostadsrättsföreningen Teleterrassen 1",
  "headerImage": "header.png"
}
```

### Page Content (pages/*.json)

Most pages follow this structure:

```json
{
  "title": "Page Title",
  "sections": [
    {
      "heading": "Section Heading",
      "content": "Section content text..."
    }
  ]
}
```

The `about.json` page has a different structure with specific fields for building information.

The `contact.json` page has fields for contact details and property manager information.

### Documents (documents/documents.json)

```json
{
  "documents": [
    {
      "type": "årsredovisning",
      "year": 2023,
      "title": "Årsredovisning 2023",
      "file": "arsredovisning-2023.pdf",
      "notes": "Optional notes"
    }
  ]
}
```

**Document types:**
- `årsredovisning` - Annual reports
- `stadgar` - Bylaws
- `energideklaration` - Energy declarations
- `övrigt` - Other documents

**Fields:**
- `type` (required): Document type
- `title` (required): Display title
- `file` (required): Filename (must exist in `content/documents/`)
- `year` (optional): Year (used for sorting)
- `notes` (optional): Additional notes

Documents are automatically grouped by type and sorted by year (descending) on the generated page.

## Adding Documents

1. Place the PDF file in `content/documents/`
2. Add an entry to `content/documents/documents.json`
3. Rebuild the site

Example:

```json
{
  "type": "årsredovisning",
  "year": 2024,
  "title": "Årsredovisning 2024",
  "file": "arsredovisning-2024.pdf"
}
```

## Changing the Header Image

1. Replace `content/static/header.png` with your image
2. Update `headerImage` in `content/site.json` if you use a different filename
3. Rebuild the site

The header image will be automatically scaled to fit. Recommended size: 1200x300 pixels.

## Development

### Format Code

```bash
./scripts/format.sh
```

This uses ormolu to format all Haskell source files.

### Verify Build

```bash
./scripts/verify.sh
```

This runs a full build and checks for errors and warnings.

### Live Reload Development

```bash
./ghcid.sh
```

Uses ghcid to watch for changes and rebuild automatically.

### Run Tests

```bash
cabal test
```

## Publishing the Site

After running the generator:

1. The `web/` directory contains the complete static site
2. Upload the contents of `web/` to your web hosting
3. Ensure your web server serves `index.html` as the default page

The site requires no server-side processing - it's purely static HTML, CSS, and files.

## Architecture

- **Models** - Data types with JSON parsing
- **Content** - Load and validate content from `content/`
- **HTML** - Plain Text HTML generation (no DSL)
- **Pages** - Page generators for each page type
- **Assets** - Copy static files and documents
- **Generator** - Main orchestration

## Design Principles

- All input must come from `content/`
- All output goes to `web/`
- No templating libraries - plain Text concatenation
- Clear error messages for invalid JSON or missing files
- Predictable, reproducible builds
- Valid HTML5 output
