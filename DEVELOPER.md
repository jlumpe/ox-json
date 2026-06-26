# Developer Guide

## Project overview

`ox-json` is a JSON export backend for Emacs Org mode, written in Emacs Lisp. It exports an Org document's syntax tree to a structured JSON representation. The package is distributed through [MELPA](https://melpa.org/#/ox-json).


### File layout

| Path | Description |
|------|-------------|
| `ox-json.el` | Package entry point — loads all other source files |
| `ox-json-core.el` | Core data types and backend registration |
| `ox-json-utils.el` | Internal utility functions |
| `ox-json-encode.el` | JSON encoding functions |
| `ox-json-export.el` | Org element export handlers |
| `ox-json-docs.org` | User-facing documentation (Org format) |
| `tests/` | ERT test suite and helpers |
| `tests/test.org` | Sample Org document used by the export test |
| `tests/test.json` | Reference JSON export of `test.org` (generated, checked in) |
| `tests/export.el` | Script to regenerate `test.json` from `test.org` |
| `Eask` | Package metadata, dependencies, and Eask build config |
| `Makefile` | Additional automation (interactive testing, export regeneration) |
| `.github/workflows/ci.yml` | GitHub Actions CI config |


### Dependencies

Runtime (declared in the `Package-Requires` header of `ox-json.el` and `depends-on` in `Eask`):

- Emacs >= 26.1
- `org` >= 9
- `s` >= 1.12

Development-only (declared in the `(development ...)` block of `Eask`): `package-lint`, `ellsp`.

Test-only: `ert` (bundled with Emacs).


## Running the tests

### Prerequisites

You need:

- A working `emacs` on your `$PATH`
- [Eask](https://emacs-eask.github.io/) installed

Install Eask with:

```bash
npm install -g @emacs-eask/cli
```

Or see the [Eask installation docs](https://emacs-eask.github.io/Getting-Started/Install-Eask/) for other methods.


### Quick start

```bash
# Install all dependencies (runtime + dev):
eask install-deps --dev

# Byte-compile the package:
eask compile

# Run the full test suite:
eask test ert tests/test-*.el
```


### Step by step

1. **Install dependencies** — downloads all declared dependencies (runtime and dev) into the local `.eask/` directory, which is kept out of version control.

   ```bash
   eask install-deps --dev
   ```

2. **Byte-compile:**

   ```bash
   eask compile
   ```

3. **Run the test suite:**

   ```bash
   eask test ert tests/test-*.el
   ```

   To run a specific test file:

   ```bash
   eask test ert tests/test-encode.el
   ```


### Interactive testing

To open an Emacs session with all test files and dependencies pre-loaded (so you can run tests with `M-x ert`):

```bash
make test-interactive
```


## What the tests cover

The test suite uses Emacs's built-in ERT (Emacs Lisp Regression Testing) framework. Test files live in `tests/` and follow the naming convention `test-*.el`.

| File | What it tests |
|------|---------------|
| `test-encode.el` | Low-level encoding functions: booleans, strings, numbers, tag strings, arrays, alists, plists, auto-type detection, and custom type encoders |
| `test-export.el` | Full document export — exports `tests/test.org` with `(:json-strict t)` and recursively compares the result against the reference file `tests/test.json`. Certain properties that vary across Org versions (e.g. properties introduced in 9.6 or 9.7, and `ref` which changes every export) are ignored during comparison. |
| `test-utils.el` | Internal utility functions: alist merging, plist-to-alist conversion, plist lookup across multiple plists, node detection, plist looping |
| `test-helpers.el` | Self-tests for the test helper functions themselves (`encoded=`, `json-obj`) |

Supporting files:

- `ox-json-test-helpers.el` — shared test infrastructure: sets up the export backend and `info` plist, provides `encoded=` (whitespace-insensitive JSON string comparison), `json-obj` (builds expected hash-table objects), `decode-compare` (round-trip encode-then-decode comparison), `with-json-decode-explicit` (macro that configures unambiguous JSON decoding settings), and recursive JSON structure comparison via `json-compare` (supports `:ignore` lists and pluggable object comparison functions).
- `tests/export.el` — standalone script used by `make export-test-org` to regenerate `tests/test.json`. Supports `EXPORT_STRICT=1` environment variable to export with `(:json-strict t)`.
- `tests/export/` — reference JSON exports broken down by feature (headings, markup, links, tables, lists, blocks, drawers, footnotes, timestamps, latex, babel, misc).


## Makefile

The Makefile is retained for tasks that don't have a direct Eask equivalent, and for the legacy `make`-based workflow. For standard build/test/lint operations, prefer the Eask commands above.

| Target | Description |
|--------|-------------|
| `test-interactive` | Open an Emacs session with test files loaded for interactive `M-x ert` |
| `export-test-org` | Re-export `tests/test.org` to `test.json` (use `EXPORT_STRICT=1` for strict mode) |
| `edit-test-org` | Open `tests/test.org` in Emacs with `ox-json` loaded |
| `org-version` | Print the version of `org-mode` that would be used |
| `clean` | Remove byte-compiled `.elc` files and the local `.emacs.d/elpa` |
| `install-deps` | Install package + test dependencies into `.emacs.d/elpa` (no-op when `NO_INSTALL_DEPS` is set) |
| `test` | Legacy pipeline: `install-deps` → `test-deps` → `run-tests` |
| `run-tests` | Run all `test-*.el` files via ERT in batch mode |
| `byte-compile-strict` | Byte-compile with warnings-as-errors (suppresses `docstrings`, `obsolete`, and `suspicious` categories) |
| `lint` | Run `package-lint` on `ox-json.el` |
| `checkdoc` | Run Emacs `checkdoc` on `ox-json.el` (via `tests/checkdoc-batch.el`) |


### Key Makefile variables

| Variable | Default | Description |
|----------|---------|-------------|
| `EMACS` | `emacs` | Emacs executable to use |
| `EXPORT_STRICT` | `0` | Set to `1` to enable `(:json-strict t)` when running `export-test-org` |
| `NO_INSTALL_DEPS` | _(empty)_ | Set to any non-empty value to skip dependency installation |


## CI pipeline

CI is defined in `.github/workflows/ci.yml` and runs on every push and pull request.


### Matrix

The workflow tests against multiple Emacs versions using [`purcell/setup-emacs`](https://github.com/purcell/setup-emacs):

- 26.3 (allowed to fail — Org 9.7 dropped support for Emacs 26)
- 27.2
- 28.2
- 29.1
- snapshot (latest development build)

The matrix uses `fail-fast: false` so all Emacs versions are tested even if one fails. Emacs 26.3 uses `continue-on-error: true` so its failure won't block the overall workflow.


### Steps

For each Emacs version the job:

1. Checks out the repository.
2. Installs the target Emacs version via `purcell/setup-emacs`.
3. Installs Eask via `emacs-eask/setup-eask`.
4. Runs `eask install-deps --dev` to install all dependencies.
5. Prints the installed `org-mode` version for debugging.
6. Runs `eask compile` — byte-compiles the package.
7. Runs `eask test ert tests/test-*.el` — executes the full ERT test suite.


## Linting and style checks

Two lint checks are available via Eask (not currently run in CI):

```bash
# MELPA package-lint (checks headers, dependencies, naming conventions):
eask lint package

# Emacs checkdoc (checks docstring conventions):
eask lint checkdoc
```


## Updating the reference export

The export test (`test-export.el`) compares a live export of `tests/test.org` against the checked-in reference file `tests/test.json`. If you change `ox-json.el` in a way that alters the output, or modify `tests/test.org`, you need to regenerate the reference:

```bash
make export-test-org
```

The comparison ignores properties that are known to vary across Org versions (e.g. `ref`, `creator`, and various properties introduced in Org 9.6 and 9.7). This ignore list is defined in `tests/test-export.el` in the `ignore-by-data-type` variable.


## Tips

- Eask installs dependencies into `.eask/<emacs-version>/elpa/`, keeping them completely separate from your personal `~/.emacs.d`. You can safely delete `.eask/` at any time and re-run `eask install-deps --dev` to restore it.
- Each test file adds its own directory to `load-path` via `(add-to-list 'load-path (file-name-directory load-file-name))`, so `ox-json-test-helpers` is always findable regardless of the working directory or how the file is loaded.
- The `encoded=` helper in the test suite compares JSON strings after stripping all whitespace, so formatting differences don't cause false failures.
- The Makefile `install-deps` target still works and installs into a local `.emacs.d/elpa/` (with `HOME` overridden to the project directory). Set `NO_INSTALL_DEPS` to any non-empty value to skip it when dependencies are already installed.
