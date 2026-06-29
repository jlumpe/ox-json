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
| `tests/` | ERT test suite and helpers |
| `tests/run-coverage.el` | Single-process test runner used by `make coverage` |
| `coverage/` | Coverage output directory (not checked in) |
| `tests/export/` | Org fixture files, exported JSON snapshots, and gitignored Markdown/HTML review views |
| `scripts/` | Helper scripts: `eask-docker.sh` and the `json-to-markdown.py`/`json-to-html.py` review-view generators |
| `planning/` | Planning notes, including `elements.md` (feature-coverage tracking) |
| `Eask` | Package metadata, dependencies, and Eask build config |
| `Makefile` | Additional automation (interactive testing, export regeneration) |
| `.github/workflows/ci.yml` | GitHub Actions CI config |


### Dependencies

Runtime (declared in the `Package-Requires` header of `ox-json.el` and `depends-on` in `Eask`):

- Emacs >= 27.1
- `org` >= 9.4
- `s` >= 1.12

Development-only (declared in the `(development ...)` block of `Eask`): `package-lint`, `undercover`.

Test-only: `ert` (bundled with Emacs).

Optional system tools:

- [`lcov`](https://github.com/linux-test-project/lcov) — generates HTML coverage reports from the LCOV data produced by `make coverage`. Install with `apt install lcov` (or your distro's equivalent).


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


### Test coverage

Coverage is collected via [`undercover.el`](https://github.com/undercover-el/undercover.el), which instruments the source files at load time and writes an LCOV report. Run:

```bash
make coverage
```

This will:

1. Clean any byte-compiled `.elc` files (undercover requires source, not compiled code).
2. Run the full test suite through `tests/run-coverage.el`, a single-process runner that initialises undercover before any source file is loaded.
3. Write the raw coverage data to `coverage/lcov.info`.
4. If `genhtml` (from the `lcov` package) is available, generate an HTML report at `coverage/html/index.html`.

To install `genhtml` on Debian/Ubuntu:

```bash
sudo apt install lcov
```

The `coverage/` directory is not checked in (it's in `.gitignore`).


## What the tests cover

The test suite uses Emacs's built-in ERT (Emacs Lisp Regression Testing) framework. Test files live in `tests/` and follow the naming convention `test-*.el`.

| File | What it tests |
|------|---------------|
| `test-encode.el` | Low-level encoding functions: booleans, strings, numbers, tag strings, arrays, alists, plists, auto-type detection, and custom type encoders |
| `test-export.el` | Full document export — defines one ERT test per `.org` file in `tests/export/`, exports it with the deterministic options in `ox-json-test-export-options` (`:json-strict t`, `:json-deterministic-refs t`, fixed author/date/email), and compares against its saved `.json` snapshot; also checks export invariants. Because refs and metadata are made deterministic, only properties that genuinely vary are ignored during comparison: `creator` (Emacs version), `pre-blank`/`post-blank` (vary in CI), and various properties introduced in Org 9.5/9.6/9.7. |
| `test-utils.el` | Internal utility functions: alist merging, plist-to-alist conversion, plist lookup across multiple plists, node detection, plist looping |
| `test-helpers.el` | Self-tests for the test helper functions themselves (`encoded=`, `json-obj`) |

Supporting files:

- `ox-json-test-helpers.el` — shared test infrastructure: sets up the export backend and `info` plist, provides `encoded=` (whitespace-insensitive JSON string comparison), `json-obj` (builds expected hash-table objects), `decode-compare` (round-trip encode-then-decode comparison), `with-json-decode-explicit` (macro that configures unambiguous JSON decoding settings), and recursive JSON structure comparison via `json-compare` (supports `:ignore` lists and pluggable object comparison functions).


### Example files

Example `.org` files in `tests/export/` are intended to cover the entire set of Org elements and
syntax. The list of features is also tracked in `planning/elements.md`.

The corresponding exported JSON files are also present in the same directory and in version control.
`tests/test-export.el` tests that the result of exporting each `.org` file matches the saved `.json`
file. The `tests/update-exports.el` script re-exports all `.org` files and overwrites the `.json`
files.


## Makefile

The Makefile is a thin facade over Eask — short `make` commands for local development. CI calls `eask` directly. All targets depend on `install-deps`, which uses a stamp file under `.eask/` (via `eask install-deps --dev`).

To test against a specific Emacs version without installing it locally, set `EASK_DOCKER` to a
dotted version (e.g. `28.2`). Make then runs eask inside a
[silex/emacs](https://hub.docker.com/r/silex/emacs) container with the project bind-mounted
(requires Docker). Because eask in the image is installed under `/root`, the container runs as
root and `scripts/eask-docker.sh` reassigns ownership of the project tree to your UID/GID when
the command finishes. Dependencies are tracked per version via `.eask/.stamp` or
`.eask/.stamp-<version>`.

```bash
EASK_DOCKER=28.2 make test
EASK_DOCKER=29.1 make org-version
```

| Target | Description |
|--------|-------------|
| `install-deps` | Install runtime + dev dependencies into `.eask/` (no-op when `NO_INSTALL_DEPS` is set) |
| `test` | Run the full ERT test suite |
| `run-tests` | Same as `test` without implying a fresh install when deps are present |
| `byte-compile` | Byte-compile the package |
| `byte-compile-strict` | Byte-compile with warnings as errors (suppresses `docstrings`, `obsolete`, and `suspicious` categories) |
| `lint` | Run `package-lint` on all `ox-json*.el` files |
| `checkdoc` | Run `checkdoc` on all `ox-json*.el` files |
| `test-interactive` | Open an Emacs session with test files loaded for interactive `M-x ert` |
| `coverage` | Run tests with `undercover.el` and write `coverage/lcov.info`; generates HTML with `genhtml` if available |
| `emacs` | Open Emacs with the project and test load-path configured |
| `org-version` | Print the version of `org-mode` in use |
| `clean` | Remove byte-compiled `.elc` files, `.eask/`, and `coverage/` |
| `update-exports` | Re-export all `.org` fixtures in `tests/export/` to `.json` snapshots |
| `update-exports-pretty` | Regenerate the gitignored `.md`/`.html` review views from the `.json` snapshots |


### Key Makefile variables

| Variable | Default | Description |
|----------|---------|-------------|
| `EASK` | `eask` | Eask executable |
| `EASK_DOCKER` | _(empty)_ | Emacs version for Docker (e.g. `28.2`); runs eask in `silex/emacs:<version>-eask` |
| `NO_INSTALL_DEPS` | _(empty)_ | Set to any non-empty value to skip dependency installation |
| `TESTS_REGEXP` | _(empty)_ | When set, filter ERT tests by name (default runs all via `eask test ert`) |


## CI pipeline

CI is defined in `.github/workflows/ci.yml` and runs on every push and pull request.


### Matrix

The workflow tests against multiple Emacs versions (the comments note the bundled Org version):

- 27.2 (Org 9.4) — minimum supported version
- 28.2 (Org 9.5)
- 29.4 (Org 9.6)
- 30.2 (Org 9.7)
- snapshot (latest development build) — allowed to fail

The matrix uses `fail-fast: false` so all Emacs versions are tested even if one fails. The per-job
`continue-on-error: ${{ matrix.allow-failure }}` setting lets the snapshot job fail
without blocking the overall workflow.


### Steps

The [`purcell/setup-emacs`](https://github.com/purcell/setup-emacs) action is **not** used directly. That action installs Nix and fetches the Emacs flake in a single step, and the flake fetch frequently hits GitHub API rate limits (HTTP 429), which fails the whole step with no opportunity to retry just the part that failed. Instead the workflow splits the action's two operations into separate steps and reuses only the action's `install-nix.sh` script, so that the throttling-prone flake fetch can be retried on its own without reinstalling Nix each time.

Each Emacs version is provided by the [`purcell/nix-emacs-ci`](https://github.com/purcell/nix-emacs-ci) flake, so the job has to set up Nix first. For each Emacs version the job:

1. Checks out the repository, plus a checkout of `purcell/setup-emacs` (pinned to `v8.0`) whose `install-nix.sh` script is reused.
2. Installs Nix via that `install-nix.sh` script (skipped if Nix is already present). This part succeeds reliably, so it's kept separate from the flake fetch below.
3. Installs the target Emacs from the `nix-emacs-ci` flake with `nix profile install`, wrapped in `nick-fields/retry` (3 attempts, staggered) because the flake fetch is the operation that actually hits GitHub's 429 throttling.
4. Installs Eask via `emacs-eask/setup-eask` (`snapshot`).
5. Runs `eask install-deps --dev` to install all dependencies.
6. Prints the installed `org-mode` version for debugging.
7. Runs `eask compile` — byte-compiles the package.
8. Runs `eask test ert tests/test-*.el` — executes the full ERT test suite.


## Linting and style checks

Two lint checks are available via Eask (not currently run in CI):

```bash
# MELPA package-lint (checks headers, dependencies, naming conventions):
eask lint package

# Emacs checkdoc (checks docstring conventions):
eask lint checkdoc
```


## Updating export snapshots

The export tests (`test-export.el`) compare live exports of each `.org` file in `tests/export/` against the checked-in `.json` snapshots. If you change `ox-json.el` in a way that alters the output, or modify a fixture file, regenerate the snapshots:

```bash
make update-exports
```

The comparison ignores properties that are known to vary across Org versions or export runs (e.g. `creator`, `pre-blank`/`post-blank`, and various properties introduced in Org 9.5/9.6/9.7). This ignore list is defined in `tests/test-export.el` in the `ignore-by-data-type-version` variable, keyed by Org version so that entries only apply on the versions where they are needed. (Refs, author, date, and email do not need ignoring because `ox-json-test-export-options` makes them deterministic.)


## MELPA and releasing

`ox-json` is distributed through [MELPA](https://melpa.org/#/ox-json). There are two MELPA channels and they behave very differently:

- **MELPA (unstable)** builds automatically from the **latest commit on the default branch** (`main`) whenever it pulls from GitHub. There is no manual publish step — anything merged to `main` becomes the next MELPA build, usually within a day.
- **[MELPA Stable](https://stable.melpa.org/#/ox-json)** builds from the **most recent git tag** that looks like a version number. A new stable release is published only when a corresponding tag is pushed to GitHub.

### Branching implications

Because plain MELPA tracks `main` directly, **anything broken on `main` ships to users automatically**. To avoid this:

- Do day-to-day development on **feature branches**, not `main`.
- Only merge into `main` once the work is **relatively stable** — compiling cleanly, passing tests, and not mid-refactor.
- Treat `main` as releasable at all times.

### Cutting a release

A release is just a version-number git tag (e.g. `v0.4.0`) pushed to GitHub; MELPA Stable picks it up on its next build. Before tagging, work through the checklist below.

#### Pre-release checklist

1. **Make sure `main` is up to date** with all changes intended for the release.
2. **Update the version number** — keep these in sync:
   - the `;; Version:` header in `ox-json.el`
   - the version string in `Eask`
3. **Update `CHANGELOG.md`** — rename the top `## Dev` section to the new version number (and add a fresh empty `## Dev` section above it for future work). Review that the entries accurately describe what changed since the last release.
4. **Byte-compile cleanly:**
   ```bash
   make byte-compile-strict
   ```
   Fix any warnings not in the deliberately-suppressed categories (`docstrings`, `obsolete`, `suspicious`).
5. **Run the linters** and fix reported problems:
   ```bash
   make lint       # package-lint — MELPA header/dependency/naming checks
   make checkdoc   # checkdoc — docstring conventions
   ```
   `package-lint` in particular gates MELPA acceptance, so it must be clean.
6. **Run the full test suite**, ideally against multiple Emacs/Org versions:
   ```bash
   make test
   EASK_DOCKER=30.2 make test   # repeat for other supported versions
   ```
7. **Regenerate export snapshots** if export output changed, and confirm the diff is intentional:
   ```bash
   make update-exports
   ```
8. **Review the docs** (`README.md`, `DEVELOPER.md`, `AGENTS.md`) for anything made inaccurate by the release.
9. **Commit** the version bump and changelog, then **tag and push:**
   ```bash
   git tag v0.4.0
   git push origin main --tags
   ```

After pushing the tag, confirm the new version appears on [MELPA Stable](https://stable.melpa.org/#/ox-json) once it rebuilds.


## Tips

- Eask installs dependencies into `.eask/<emacs-version>/elpa/`, keeping them completely separate from your personal `~/.emacs.d`. You can safely delete `.eask/` at any time and re-run `make install-deps` or `eask install-deps --dev` to restore it.
- Each test file adds its own directory to `load-path` via `(add-to-list 'load-path (file-name-directory load-file-name))`, so `ox-json-test-helpers` is always findable regardless of the working directory or how the file is loaded.
- The `encoded=` helper in the test suite compares JSON strings after stripping all whitespace, so formatting differences don't cause false failures.
