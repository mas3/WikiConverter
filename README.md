# WikiConverter

A library for Delphi that converts Markdown and PukiWiki text to HTML.


## Overview

Convert the following text to HTML:

- Markdown: Most of GitHub Flavored Markdown (GFM) is supported
- PukiWiki: Some PukiWiki formatting rules are supported


## Requirement

Delphi 12.1

If you want to run tests you need DUnitX.


## Usage

Add the files under the MyWiki folder to your project.

Uses WikiConverter and the Converter that corresponds to the type of text you want to convert.

```pascal
uses
  MyWiki.WikiConverter, MyWiki.MarkdownConverter;
```

Create a Converter.

```pascal
var
  Converter: TWikiConverter
begin
  Converter := TMarkdownConverter.Create;
```

The WikiToHtml method takes the text you want to convert and returns HTML.

```pascal
  Html := Converter.WikiToHtml(Markdown);
```


## Supported notations

### Markdown

GFM support status:

- [x] Thematic breaks
- [x] ATX headings
- [x] Setext headings
- [x] Indented code blocks
- [x] Fenced code blocks
- [x] HTML blocks
- [x] Link reference definitions
- [x] Paragraphs
- [x] Blank lines
- [x] Tables
- [x] Block quotes
- [x] List items
- [x] Task list items
- [x] Backslash escapes
- [x] Entity and numeric character references
- [x] Code spans
- [x] Emphasis and strong emphasis
- [x] Strikethrough
- [x] Links
- [x] Images
- Autolinks
  - [x] < > link
  - [ ] extended www autolink
  - [x] extended url autolink
  - [ ] extended email autolink
  - [ ] extended protocol autolink
- [x] Raw HTML
- [ ] Disallowed Raw HTML
- [x] Hard line breaks
- [x] Soft line breaks
- [x] Textual content

Footnotes:

You can use footnotes by using the GitHub format.

```
This is a sample footnote[^1].

[^1]: This sentence will appear in the footnote.
```

Page link:

Enclosing it in [[ and ]] will create a page link. The page name and link destination must be set in advance.

```pascal
PageList := TWikiPages.Create;
PageList.Add('Example', 'https://example.com');

Converter := TMarkdownConverter.Create;
Converter.SetPageList(FPageList);
```

```
Example site is [[Example]].
```

Give ID to the header:

Assigning True to the GiveIdToHeader property will give the header an ID. The assigned ID can be obtained from the Header.HeaderList property.

```pascal
Converter.GiveIdToHeader := True;
```


## Related apps

You can check the conversion operation with the following applications.

- [myWiki](https://mas3lab.net/soft/mywiki/index_en.html) - Memo app that supports Markdown
- [myMarkdownViewer](https://mas3lab.net/soft/mymarkdownviewer/index_en.html) - Markdown File Viewer


## Author

MASUDA Takashi <https://mas3lab.net/>

