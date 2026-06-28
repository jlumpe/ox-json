# Supported Org elements and features

## In example files

Features in example `*.org` files in `tests/export/`. A check indicates that the feature is exported
properly.


### Markup

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

#### 📝 To add

- [ ] Subscript/superscript with complex braced content (e.g. `x_{n+1}`)
- [ ] Subscript/superscript with markup inside braces


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


#### ⛔ Problems

- **Integer priority values**: [docs](https://orgmode.org/manual/Priorities.html) state integers can
  be two digits (up to 64), but the raw org property value is a character code, so unsure how that
  would work. Headline with priority 10 does not include the property at all. May need to set
  `org-priority-highest`/`org-priority-lowest`.
- **Multiple planning keywords under single headline**: Only the first seems to be included in
  headline properties, the rest appear in the headline's section contents as a paragraph. This
  appears to be an Org mode problem.

#### 📝 To add

- [ ] `#+SEQ_TODO:` — custom TODO keyword sequence
- [ ] `#+TYP_TODO:` — separate type vs action TODO keywords


### Lists

- Unordered lists
  - [x] `-` bullet
  - [x] `+` bullet
  - [x] `*` bullet (only allowed if nested)
- Ordered lists
  - [x] `1.` style
  - [x] `1)` style
  - [x] Override number (`[@20]`, exported as `counter` property, omitted from tested paragraph)
- [x] Description list (`term :: definition`) - term appears in `tag` property, definition in
  `paragraph` element in `contents`.
- [x] Nested lists
- Checkbox items
  - [x] Unchecked (`[ ]`) — exported as `"checkbox": "off"`
  - [x] Checked (`[X]`) — exported as `"checkbox": "on"`
  - [x] Partial (`[-]`) — exported as `"checkbox": "trans"`

Note: bullet always exported verbatim, even out-of-order ordered bullets without counter.

#### 📝 To add

- [ ] Paragraph separation and blank lines within list items


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

#### 📝 To add

- [ ] Plain link — bare URL (e.g. `https://example.com`); `format: "plain"`
- [ ] Angle link — `<https://example.com>`; `format: "angle"`
- [ ] Protocol links — `id:`, `doi:`, `info:`, `elisp:`, `shell:`, etc.; distinct `type`
  property on `link` node
- [ ] File link with search option — e.g. `[[file:foo.org::*Heading]]`,
  `[[file:foo.org::123]]`; `search-option` property on `link` node


### Tables

**⚠️Needs human review⚠️**

- [x] Basic table (header row + rule row + data rows)
- [x] Column groups — `<`, `>`, `<>` markers present as cell content
- [ ] Table formula (`#+TBLFM:`)

#### ⛔ Problems

- **`#+TBLFM:` not linked to table**: The formula is exported as a standalone
  `keyword` node (`key: "TBLFM"`) that precedes the table in the section. The
  table node's own `tblfm` property is `null`.

#### 📝 To add

- [ ] Column width and alignment markers — `<l>`, `<r>`, `<c>`, `<N>` in column group rows
- [ ] table.el table — `table` node with `type: "table.el"`


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

#### ⛔ Problems

- **Source block with `:exports results`**: The source block in `blocks.org` uses
  `:exports results`, so the `src-block` node is not exported — only the
  evaluated result appears, as a `fixed-width` node with `value: "3"`. The
  `src-block` element type is not represented here (it is present in `babel.md`
  via the named blocks).
- **Comment block not exported**: The `#+begin_comment` block content is
  intentionally excluded from export; no `comment-block` node appears.

#### 📝 To add

- [ ] Inline source block — `src_LANG{code}` or `src_LANG[options]{code}`; `inline-src-block`
  node
- [ ] Line numbers in source/example blocks — `-n` / `+n` switches; `number-lines` property
- [ ] Coderefs — `(ref:name)` label in block + `[[(name)]]` link; `label` property on nodes


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

#### 📝 To add

- [ ] Catch-up repeater (`++1w`) — `"repeater-type": "catch-up"`


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

#### ⛔ Problems

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

#### ⛔ Problems

- **Inline call evaluated, not represented**: `call_add-one()` is evaluated and
  its result (`3`) is exported as a `verbatim` node. No `inline-babel-call` node
  appears in the output.
- **`#+CALL:` evaluated, not represented**: The call line result is exported as a
  `fixed-width` node with `value: "3"`. No `babel-call` node appears.

#### 📝 To add

- [ ] Source block properties — `:language`, `:switches`, `:parameters`, `:number-lines`,
  `:retain-labels`, etc. on `src-block` node
- [ ] `#+NAME:` on non-src elements (tables, paragraphs, etc.)
- [ ] `#+RESULTS:` keyword
- [ ] Noweb references — `<<block>>` inside src blocks


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

#### ⛔ Problems

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

#### 📝 To add

- [ ] `#+INCLUDE:` directive — include content from another file
- [ ] `#+STARTUP:` options — parsing options (may not produce export nodes)


## To add

Features not yet covered by any `tests/export/*.org` fixture. Add a new example file (or files)
when implementing.

### Citations

- [ ] Citation reference — `[cite:@key]`; `citation-reference` node
- [ ] Citation with style — `[cite/style:@key]`; `citation` node

### Affiliated keywords

Element metadata attached to the following element (e.g. table, block, link).

- [ ] `#+CAPTION:` — `caption` affiliated property
- [ ] `#+ATTR_HTML:` — `attr_html` affiliated property
- [ ] `#+ATTR_LATEX:` — `attr_latex` affiliated property
- [ ] Other `#+ATTR_*:` backends — e.g. `#+ATTR_JSON:`

### File-level properties

- [ ] `#+PROPERTY:` — file-level property inheritance and defaults
