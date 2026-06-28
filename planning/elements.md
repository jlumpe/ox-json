# Supported Org elements and features

## In example files

Features in example `*.org` files in `tests/export/`. A check indicates that the feature is exported
properly.


### Markup

**⚠️Needs human review⚠️**

- Basic:
  - [x] bold
  - [x] italic
  - [x] underline
  - [x] strike-through
  - [x] verbatim
  - [x] code
- Super- and sub-scripts
  - [x] Superscript
  - [x] Subscript
  - [x] With and without braces
- [x] Entities
- [x] Line breaks


### Headings

- Heading levels
  - [x] Level 1
  - [x] Level 2
  - [x] Level 3
  - [x] Level 4
  - [x] Level 5
  - [x] Level 6
- [x] Markup
- [x] TODO states
  - [x] TODO
  - [x] DONE
  - [x] Custom state
- [ ] Priority cookies
  - [x] Character values - exported as strings, not integers
  - [x] Single-digit integer values (also a string)
  - [ ] Multi-digit integer values
- [x] Tags
- [ ] Planning keywords
  - [x] Individual DEADLINE
  - [x] Individual SCHEDULED
  - [x] Individual CLOSED
  - [ ] Multiple in single headline
- [x] Statistics cookies
  - [x] Fraction (`[1/2]`)
  - [x] Percentage (`[50%]`)
- [x] Extra properties:
  - [x] `tags-all`
    - [x] Includes file tags
    - [x] Inherits parent
    - [x] Inherits + extends parent
  - (`drawers` tested in `drawers.org`)


#### Problems

- **Integer priority values**: [docs](https://orgmode.org/manual/Priorities.html) state integers can
  be two digits (up to 64), but the raw org property value is a character code, so unsure how that
  would work. Headline with priority 10 does not include the property at all. May need to set
  `org-priority-highest`/`org-priority-lowest`.
- **Multiple planning keywords under single headline**: Only the first seems to be included in
  headline properties, the rest appear in the headline's section contents as a paragraph. This
  appears to be an Org mode problem.


### Lists

**⚠️Needs human review⚠️**

- Unordered lists
  - [x] `-` bullet
  - [x] `+` bullet
  - [ ] `*` bullet
- Ordered lists
  - [x] `1.` style
  - [x] `1)` style
- [ ] Description list (`term :: definition`)
- [x] Nested lists
- Checkbox items
  - [x] Unchecked (`[ ]`) — exported as `"checkbox": "off"`
  - [x] Checked (`[X]`) — exported as `"checkbox": "on"`
  - [x] Partial (`[-]`) — exported as `"checkbox": "trans"`

#### Problems

- **`*` bullet parsed as headline**: In `lists.org`, `* Item three` is parsed by
  org as a level-1 headline rather than an unordered list item, so the `*` bullet
  type is never represented as a list `item` node.
- **Description list not parsed as `plain-list`**: `Term one :: Definition one`
  and `Term two :: Definition two` are exported as a plain text paragraph
  (`"Term one :: Definition one\nTerm two :: Definition two\n"`) rather than as a
  `plain-list` of type `descriptive` with `tag` entries.


### Footnotes

**⚠️Needs human review⚠️**

- [x] Labeled footnote reference (`[fn:label]`) — `type: "standard"`
- [x] Inline footnote (`[fn:: text]`) — `type: "inline"`, `label: null`
- [x] Named inline footnote (`[fn:name: text]`) — `type: "inline"`, `label: "name"`
- [x] Footnote definitions — `footnote-definition` nodes with `label` property


### Links

- External links
  - [x] With description
  - [x] Without description
- File links
  - [x] With description
  - [x] Without description
- [x] Internal link — `target` node + `link` with `type: "fuzzy"`; `is-internal`
  and `target-ref` are top-level fields on the link node
- [x] Custom ID link (`[[#custom-id]]`) — `type: "custom-id"`, `target-ref` to headline
- [x] Radio target (`<<<target>>>`) and radio link — `radio-target` node + `link`
  with `type: "radio"`
- Added properties:
  - [x] `is-inline-image`: true in "Image link" section
  - [x] `target-ref`: see internal links above
  - [x] `is-internal`


### Tables

**⚠️Needs human review⚠️**

- [x] Basic table (header row + rule row + data rows)
- [x] Column groups — `<`, `>`, `<>` markers present as cell content
- [ ] Table formula (`#+TBLFM:`)

#### Problems

- **`#+TBLFM:` not linked to table**: The formula is exported as a standalone
  `keyword` node (`key: "TBLFM"`) that precedes the table in the section. The
  table node's own `tblfm` property is `null`.


### Blocks

**⚠️Needs human review⚠️**

- [ ] Source block (`#+begin_src`)
- [x] Example block (`#+begin_example`)
- [x] Quote block (`#+begin_quote`)
- [x] Verse block (`#+begin_verse`)
- [x] Center block (`#+begin_center`)
- [x] Special block (arbitrary type) — `type: "special"` in properties
- [x] Export block (`#+begin_export`) — `type: "HTML"`, value contains raw HTML
- [ ] Comment block (`#+begin_comment`)
- [x] Fixed-width area (`: ` prefix)

#### Problems

- **Source block with `:exports results`**: The source block in `blocks.org` uses
  `:exports results`, so the `src-block` node is not exported — only the
  evaluated result appears, as a `fixed-width` node with `value: "3"`. The
  `src-block` element type is not represented here (it is present in `babel.md`
  via the named blocks).
- **Comment block not exported**: The `#+begin_comment` block content is
  intentionally excluded from export; no `comment-block` node appears.


### Timestamps

**⚠️Needs human review⚠️**

- Active timestamps
  - [x] Date only — `start` and `end` are date strings (`"2024-01-15"`),
    top-level fields on the timestamp node
  - [x] Date and time — `start`/`end` are ISO-8601 datetime strings
- Inactive timestamps
  - [x] Date only
  - [x] Date and time
- [x] Date range (`<date1>--<date2>`) — `type: "active-range"`,
  `range-type: "daterange"`
- [x] Time range (`<date HH:MM-HH:MM>`) — `type: "active-range"`,
  `range-type: "timerange"`; distinguishable from date ranges via `range-type`
- Repeaters
  - [x] Cumulate (`+1w`) — `"repeater-type": "cumulate"`
  - [x] Restart (`.+1m`) — `"repeater-type": "restart"`
- [x] Warning period (`-3d`) — `warning: {type: "all", unit: "day", value: 3}`
  in deadline timestamp


### Drawers

**⚠️Needs human review⚠️**

- Property drawer
  - [x] Plain text value
  - [x] Value with markup — markup stored as raw string, not parsed
  - [x] Whitespace trimming — leading/trailing whitespace stripped from value
  - [x] Accumulating property (`key+`) — values joined as `"abc (def ghi)"`
  - [ ] Case-sensitive keys
  - [x] Non-alphabetical key (`_`)
  - [x] Numeric key (`123`)
  - [x] ID property
  - [x] CUSTOM_ID property
- [ ] Custom drawer (e.g. LOGBOOK)
- [x] Arbitrary drawer with body content — `drawer` node with `drawer-name` property

#### Problems

- **Case-insensitive key normalization**: `:case_sensitive: one` and
  `:CASE_sensitive: two` both normalize to `CASE_SENSITIVE`. The second entry
  overwrites the first, yielding `"CASE_SENSITIVE": "two"`. The property key
  namespace is case-insensitive.
- **LOGBOOK drawer not exported**: The `:LOGBOOK:` drawer content is absent from
  `drawers.md` (the section has no contents block). Org treats LOGBOOK as a
  special drawer and excludes it from export.


### LaTeX

**⚠️Needs human review⚠️**

- Inline math
  - [x] `$...$` syntax — `latex-fragment` node
  - [x] `\(...\)` syntax — `latex-fragment` node
- Display math
  - [x] `$$...$$` syntax — `latex-fragment` node
  - [x] `\[...\]` syntax — `latex-fragment` node
- [x] LaTeX environment (`\begin{...}...\end{...}`) — `latex-environment` node
  with `value` containing full environment text


### Babel / Calls

**⚠️Needs human review⚠️**

- [x] Named source block (`#+name:`) — `"name": "add-one"` in `src-block` properties
- [ ] Inline call syntax (`call_name()`)
- [ ] Call line syntax (`#+CALL:`)

#### Problems

- **Inline call evaluated, not represented**: `call_add-one()` is evaluated and
  its result (`3`) is exported as a `verbatim` node. No `inline-babel-call` node
  appears in the output.
- **`#+CALL:` evaluated, not represented**: The call line result is exported as a
  `fixed-width` node with `value: "3"`. No `babel-call` node appears.


### Misc

**⚠️Needs human review⚠️**

- Document keywords
  - [x] `#+title:` — in document `properties.title` array and as `keyword` node
  - [x] `#+author:` — in document `properties.author` and as `keyword` node
  - [x] `#+date:` — in document `properties.date` and as `keyword` node
  - [x] `#+filetags:` — in document `properties.filetags` array; inherited on all
    headings via `tags-all`
  - [ ] `#+description:` (with inline markup)
  - [x] `#+OPTIONS:` — exported as `keyword` node with `key: "OPTIONS"`
- [x] Macro definition (`#+MACRO:`) — `keyword` node with `key: "MACRO"`
- [x] Macro invocation (`{{{name(args)}}}`) — expanded inline; `{{{hello(World)}}}`
  becomes `"Hello, World!"`
- [x] Horizontal rule (`-----`) — `horizontal-rule` node
- [ ] Comment line (`# ...`)
- [ ] Clock entry (`CLOCK:`)
- [x] Diary sexp — `diary-sexp` node with `value` containing the sexp string
- [x] Dynamic block (`#+BEGIN: ... #+END:`) — `dynamic-block` node with
  `block-name` and `arguments`
- [ ] Inlinetask
- [x] Target (`<<name>>`) — `target` node with `value` property
- [x] Export snippet (`@@backend:content@@`) — `export-snippet` node with
  `back-end` and `value`
- [x] Arbitrary keyword (`#+KEY: value`) — `keyword` node

#### Problems

- **`#+description:` with markup not parsed into document properties**: The
  description value `"Description with /markup/ *in* _it_"` is exported as a raw
  string in a `keyword` node, but `properties.description` in the document object
  is `[]` (empty). Contrast with `title`/`author`/`date` which are populated.
- **Comment lines not exported**: `# This is a comment line` produces no node in
  the section body; the `comment` element type is absent.
- **CLOCK entries not exported**: `CLOCK: [...]--[...] => 1:00` produces no node;
  the `clock` element type is absent from the section body.
- **Inlinetask closing line misinterpreted**: The `********` closing marker of the
  inlinetask is parsed as nested bold markup rather than being recognised as an
  inlinetask end. The section body contains a deeply nested `bold` tree wrapping
  `"**"` instead of a clean inlinetask body.
