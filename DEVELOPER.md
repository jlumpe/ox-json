# Developer Guide

## Project overview

`ox-json` is a JSON export backend for Emacs Org mode, written in Emacs Lisp. It exports an Org document's syntax tree to a structured JSON representation. The package is distributed through [MELPA](https://melpa.org/#/ox-json).


### File layout

| Path | Description |
|------|-------------|
| `ox-json.el` | Main package source — the export backend |
| `ox-json-docs.org` | User-facing documentation (Org format) |
| `tests/` | ERT test suite and helpers |
| `tests/test.org` | Sample Org document used by the export test |
| `tests/test.json` | Reference JSON export of `test.org` (generated, checked in) |
| `tests/export.el` | Script to regenerate `test.json` from `test.org` |
| `Makefile` | Build, test, and lint automation |
| `.github/workflows/ci.yml` | GitHub Actions CI config |


### Dependencies

Runtime (declared in the `Package-Requires` header of `ox-json.el`):

- Emacs >= 26.1
- `org` >= 9
- `s` >= 1.12

Test-only: `ert` (bundled with Emacs), `package-lint`.


## Running the tests

### Prerequisites

You need a working `emacs` on your `$PATH`. The Makefile defaults to the `emacs` command but you can override it:

```bash
make EMACS=/path/to/emacs test
```


### Quick start

```bash
# Install deps, check test deps load, and run the full suite:
make test
```

This is equivalent to running `install-deps`, `test-deps`, and `run-tests` in sequence.


### Step by step

1. **Install dependencies** — downloads package and test dependencies into a local `.emacs.d/elpa` directory (kept out of version control via `.gitignore`). The `HOME` variable is overridden to the working directory so this `.emacs.d` is used instead of your real one.

   ```bash
   make install-deps
   ```

   Under the hood this runs `tests/install-deps.el`, which reads the `Package-Requires` header from `ox-json.el`, refreshes the MELPA package index, and installs everything.

2. **Verify test dependencies load** — a quick smoke test that each test dep (`ert`) can be `require`'d:

   ```bash
   make test-deps
   ```

3. **Run the test suite:**

   ```bash
   make run-tests
   ```

   You can filter tests by name with a regex:

   ```bash
   make run-tests TESTS_REGEXP=encode
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


## Makefile targets

| Target | Description |
|--------|-------------|
| `install-deps` | Install package + test dependencies into `.emacs.d/elpa` (no-op when `NO_INSTALL_DEPS` is set) |
| `test` | Full pipeline: `install-deps` → `test-deps` → `run-tests` |
| `run-tests` | Run all `test-*.el` files via ERT in batch mode |
| `test-deps` | Verify each test dependency can be loaded |
| `test-interactive` | Open an Emacs session with test files loaded for interactive `M-x ert` |
| `byte-compile` | Byte-compile all `.el` files (warnings shown but not fatal) |
| `byte-compile-strict` | Byte-compile with warnings-as-errors (suppresses `docstrings`, `obsolete`, and `suspicious` categories) |
| `lint` | Run `package-lint` on `ox-json.el` |
| `checkdoc` | Run Emacs `checkdoc` on `ox-json.el` (via `tests/checkdoc-batch.el`) |
| `export-test-org` | Re-export `tests/test.org` to `test.json` (use `EXPORT_STRICT=1` for strict mode) |
| `edit-test-org` | Open `tests/test.org` in Emacs with `ox-json` loaded |
| `org-version` | Print the version of `org-mode` that would be used |
| `emacs` | Start an Emacs session with the same package config and load paths used for tests |
| `clean` | Remove byte-compiled `.elc` files and the local `.emacs.d/elpa` |


### Key Makefile variables

| Variable | Default | Description |
|----------|---------|-------------|
| `EMACS` | `emacs` | Emacs executable to use |
| `TESTS_REGEXP` | _(empty — all tests)_ | Regex to filter which tests to run |
| `EXPORT_STRICT` | `0` | Set to `1` to enable `(:json-strict t)` when running `export-test-org` |
| `NO_INSTALL_DEPS` | _(empty)_ | Set to any non-empty value to skip dependency installation (useful in containers with pre-installed deps) |


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
2. Installs the target Emacs version.
3. Runs `make install-deps test-deps` to install and verify dependencies.
4. Prints the installed `org-mode` version for debugging.
5. Runs `make byte-compile-strict` — byte-compiles with warnings treated as errors (excluding `docstrings`, `obsolete`, and `suspicious` categories).
6. Runs `make run-tests` — executes the full ERT test suite.


## Linting and style checks

Two lint targets are available (not currently run in CI):

```bash
# MELPA package-lint (checks headers, dependencies, naming conventions):
make lint

# Emacs checkdoc (checks docstring conventions):
make checkdoc
```


## Updating the reference export

The export test (`test-export.el`) compares a live export of `tests/test.org` against the checked-in reference file `tests/test.json`. If you change `ox-json.el` in a way that alters the output, or modify `tests/test.org`, you need to regenerate the reference:

```bash
make export-test-org
```

The comparison ignores properties that are known to vary across Org versions (e.g. `ref`, `creator`, and various properties introduced in Org 9.6 and 9.7). This ignore list is defined in `tests/test-export.el` in the `ignore-by-data-type` variable.


## Tips

- The Makefile sets `HOME` to the project working directory so that dependency installation uses a local `.emacs.d/` rather than your real home directory. This is intentional — don't be surprised if `~/.emacs.d` is unaffected.
- `byte-compile-strict` (used in CI) suppresses the `docstrings`, `obsolete`, and `suspicious` warning categories but treats all other warnings as errors. The plain `byte-compile` target shows warnings without failing.
- The `encoded=` helper in the test suite compares JSON strings after stripping all whitespace, so formatting differences don't cause false failures.
- Set `NO_INSTALL_DEPS` to a non-empty value when running in a container or environment where dependencies are already installed to skip the install step.
