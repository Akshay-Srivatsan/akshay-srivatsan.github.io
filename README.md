# aks.io

A multilingual personal website and blog built with Hugo 0.161.1. The generator
needs only the Hugo binary: no Node, Python, Haskell, external theme, or package
installation step.

## Build and preview

Install Hugo 0.161.1, or use the existing Nix environment:

```sh
nix develop
make build
make serve
```

`make build` writes the static site to `public/`. `make serve` (also `make watch`)
starts Hugo's development server with automatic rebuilding. `make rebuild`
rebuilds and removes stale output; `make clean` removes `public/`.

To preview unpublished drafts and future posts:

```sh
hugo server --buildDrafts --buildFuture
```

`make check` builds the site, checks local links and copied assets, and exercises
all 15 nonempty combinations of supported post languages with temporary fixtures.
It also checks archives, tags, RSS, publication dates, drafts, and shared links.
Checks require `xmllint` from libxml2, included in `nix develop` (on Ubuntu,
install `libxml2-utils`). Fixtures never enter `public/`.

GitHub Actions runs these checks and deploys `public/` on pushes to `master`.

## Content pages

Write Markdown with YAML front matter. Languages come from the filename: `en`,
`la`, `sa`, or `ta`. Tamil and Sanskrit are written directly in Tamil and
Devanagari; there is no transliteration step.

- Homepages: `content/_index.en.md`, and the corresponding `.la.md`, `.sa.md`,
  and `.ta.md` files.
- Other pages: `content/about.en.md`, `content/about.ta.md`, etc. Only create
  the translations you want to publish.
- Shared static files: `static/`. Its contents are copied to the website root;
  for example, `static/assets/pdf/osdi26.pdf` is served as `/assets/pdf/osdi26.pdf`.

English URLs have no language prefix; other languages use `/la/`, `/sa/`, and
`/ta/`. Existing language `.html` URLs are retired without redirects. Missing
content translations have no generated page. Switchers link only to translations
that exist.

Existing content pages include their headings in Markdown. Blog post headings
come from front matter. Tables, definition lists, footnotes, and raw HTML are
supported. For a passage in another language, use `<span lang="en">text</span>`
or a `<div lang="en">` block with blank lines around the Markdown inside it.

## Blog posts

Each post is a Hugo page bundle. Create it with:

```sh
hugo new content --kind blog blog/my-post/index.en.md
```

Or create `content/blog/my-post/index.ta.md` directly:

```markdown
---
title: A post title
date: 2026-10-01
draft: true
tags: [systems, programming]
---

Write the post here.
```

Put translations beside the first file, such as `index.la.md` and `index.sa.md`.
Any nonempty subset of languages is supported; an English version is optional.
The bundle name (`my-post`) identifies the post across languages. All translations
must have the same publication date. Titles and tags can vary by language.
Remove `draft: true` when that translation is ready. Production builds exclude
drafts and future posts, including their tag and translation links.

Published post URLs are `/blog/my-post/`, `/ta/blog/my-post/`, etc. The name
`archive` is reserved for the archive section. Images can live beside the post
and be linked with normal relative Markdown paths, such as `![Description](photo.jpg)`.

Every language's blog lists all published posts once, newest first. It prefers
the reader's language, then English, Latin, Sanskrit, and Tamil. Each entry links
to all published translations. `/blog/archive/` and its translated counterparts
group the same posts by year and month. RSS at `/blog/index.xml` (and translated
counterparts) uses the same selection and stable identifiers across translations.

Tags use Hugo's native taxonomy: `/tags/` and `/tags/systems/`, with language
prefixes where appropriate. Each tag page lists only posts authored in that
language. Tags may be translated independently; there is no shared tag registry.

## Shared links

Frequently used URLs live in `data/links.yaml`, once for all languages:

```yaml
keithw: https://cs.stanford.edu/~keithw/
portrait: /assets/img/portrait_2026.jpg
```

Use the `link` shortcode as a normal Markdown link or image destination. The label
stays in the page's language:

```markdown
[Keith Winstein]({{< link "keithw" >}})
![A portrait]({{< link "portrait" >}})
```

Updating a registry entry updates every reference. An unknown key fails the build
with the source location. One-off URLs and asset paths can remain ordinary
Markdown links. For internal content pages, use Hugo's built-in `ref` shortcode:

```markdown
[Shows]({{< ref "/shows" >}})
```

Site settings and language order live in `hugo.yaml`; interface labels live in
`i18n/`. HTML templates live in `layouts/`, with one small shared helper for
selecting multilingual blog posts.
