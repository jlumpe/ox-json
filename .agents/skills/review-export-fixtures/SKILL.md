---
name: review-export-fixtures
description: >-
  Verify ox-json test export fixtures against planning/elements.md. Compare
  tests/export/*.org with *.md or *.json output, update checkboxes and Problems
  sections, classify fixture vs exporter vs Org bugs. Use when reviewing or
  verifying test exports, export fixtures, elements.md status, or comparing
  .org/.md/.json in tests/export/.
---

# Review export fixtures

Workflow for checking whether `ox-json` export matches intent in `tests/export/` and recording status in `planning/elements.md`.

## Prerequisites

Regenerate artifacts before review if export code or `.org` fixtures changed:

```bash
make update-exports          # .org → .json (snapshot tests)
make update-exports-pretty   # .json → .md / .html (review views)
```

`.md` and `.html` are gitignored; read `.json` if pretty views are missing.

## Review workflow

For each feature file requested by the user (e.g. `lists.org`):

1. **Read the fixture** — `tests/export/<name>.org`. Each top-level `* Headline` is one test case; note what Org element or property it exercises.
2. **Read the export** — prefer `tests/export/<name>.md` for human review; fall back to `<name>.json`. Top-level Org headlines map to `##` sections in `.md`.
3. **Compare to checklist** — open the matching section in `planning/elements.md` (e.g. `### Lists` for `lists.org`).
4. **Record outcome**:
   - JSON matches intent → check the box (`[x]`).
   - Wrong or incomplete → leave unchecked; add or update `#### Problems` under that feature group with a short explanation.
5. **Classify the cause** when something fails:
   - **Fixture bug** — invalid Org in `.org` (fix the fixture, then re-run `make update-exports`).
   - **Exporter bug** — Org parses correctly but JSON is wrong or missing fields (fix `ox-json-*.el`).
   - **Org limitation** — org-element attaches data elsewhere (e.g. planning keywords only on headline props individually, not combined on one headline).

Repeat for all files the user asked about, or all `tests/export/*.org` if asked to verify everything.

## What to check in export

For each test case, confirm:

- Correct `type` / `$$data_type` for the element under test.
- Expected properties on the node (and on parent `headline` / `section` when relevant).
- Inline markup inside paragraph `contents` (bold, links, timestamps, etc.).
- No spurious extra nodes or wrong nesting.

Do not require fields the project deliberately omits (e.g. list `structure` was removed from export).

## Updating elements.md

- Match checkbox granularity to the checklist (parent items may stay unchecked while children are checked).
- `#### Problems` — bullet list of specific mismatches; mention fixture file and headline title when helpful.
- Remove or edit Problems entries when fixed.

## Fixture conventions

- One `*.org` per feature area; top-level headlines map 1:1 to `.md` `##` sections.
- Use valid Org syntax: tags on the headline line; description lists as `- term :: def`; nested bullets indented (not `*` at column 0 for top-level items).
- File metadata is pinned by `ox-json-test-export-options` in `tests/ox-json-test-helpers.el` — do not expect arbitrary `#+author:` in fixtures to appear in JSON.

## Reference

For `.md` layout (File properties, Headline properties, Section contents code blocks), see [reference.md](reference.md).
