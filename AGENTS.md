# Agent guide

Notes for AI agents working on `ox-json`, an Org mode JSON export backend.


## Test exports (`tests/export/`)

| File | Role |
|------|------|
| `*.org` | **Source fixtures** — hand-written; checked in |
| `*.json` | **Snapshot output** — compared by `tests/test-export.el` |
| `*.md`, `*.html` | **Review views** — from `.json`; gitignored; not used by tests |

Feature coverage: `planning/elements.md` (checkboxes + `#### ⛔ Problems` / `#### 📝 To add` sections).

```bash
make update-exports          # .org → .json
make update-exports-pretty   # .json → .md / .html
make test                    # run snapshot tests
```

After changing export code or fixtures: update exports, run tests, regenerate pretty views if reviewing, then update `planning/elements.md` as needed.

Export uses deterministic options from `tests/ox-json-test-helpers.el` (`ox-json-test-export-options`).

**Review workflow:** use the project skill `.agents/skills/review-export-fixtures/SKILL.md` when verifying fixtures, comparing `.org` to `.md`/`.json`, or updating `elements.md` checkboxes.
