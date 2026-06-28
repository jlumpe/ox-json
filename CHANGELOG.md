# CHANGELOG


## Dev

### Features

- Org mode 9.7 support ([#9](https://github.com/jlumpe/ox-json/pull/9), thanks to [tcahill](https://github.com/tcahill) for the contribution):
  - Handle deferred properties and the `:standard-properties` vector via `org-element-properties-map`
  - Skip private properties (names containing `--`)
  - Stop exporting `:post-affiliated`, `:robust-begin`, and `:robust-end`
  - Export test now compares full document output against checked-in reference JSON (`tests/test.json`).
- New export options:
  - `:json-postprocess` (`pretty`, `minimal`, or nil): to control final JSON formatting (default
    `:pretty`).
  - `:json-deterministic-refs` (`t` or nil): Make element `ref` values deterministic
    (based on structural path in the parse tree) (default nil).

### Changes to output format

- Omit the `contents` key from exported nodes when it is empty.
- Custom type-specific node properties/fields have been moved from inside `"properties"` to the
  outer level, to distinguish them from Org's builtin properties:
  - `headline` nodes: `tags-all`
  - `link` nodes: `target-ref`, `is-internal`, `is-inline-image`
  - `timestamp` nodes: `start`, `end`
- Node `properties` objects now export keys in alphabetical order.
- Omit default node property values from export. Set the `:json-omit-default-property-values` to
  `nil` to restore old behavior.

### Bug fixes

- Resolve Org 9.7 headline deferred properties (`:archivedp`, `:footnote-section-p`,
  etc.) with force-undefer when enumerating node properties. Shared deferred
  placeholders with `auto-undefer-p` nil were previously exported as `true`.
- Fix improper quoting in `cl-case` link-type branches.
- Fix incorrect argument order in `ox-json--type-error` calls.

### Other

- Split into multiple source files.
- Adopt [Eask](https://emacs-eask.github.io/) for dependency management, byte compilation, and CI.
- Add `DEVELOPER.md` developer guide.
- Add test coverage reporting via `undercover`.
- Makefile refactored as a thin facade over Eask.


## 0.3.0

- Data in property drawers is now exported with headlines in the `"drawer"` property.

### Backwards-incompatible changes

- All of the top-level document attributes (`author`, `date`, etc.) are now grouped under the
  `"properties"` key of the `org-document` object.
- Export `start` property of `timestamp` nodes with the correct name. Was being exported with the
  name `begin`.
- JSON objects representing generic key/value mappings no longer have the `"$$data_type"` property.

### Other
- Now requires Emacs 26.1.
- Reorganized/cleaned up much of the internals.


## 0.2.0

### Backwards-incompatible changes

- All of the top-level document attributes (`author`, `date`, etc.) are now grouped under the
  `"properties"` key of the `org-document` object.
- Export `start` property of `timestamp` nodes with the correct name. Was being exported with the
  name `begin`.
- JSON objects representing generic key/value mappings no longer have the `"$$data_type"` property.

### Features

- Data in property drawers is now exported with headlines in the `"drawer"` property.

### Other

- Now requires Emacs 26.1.
- Reorganized/cleaned up much of the internals.


## 0.1.0

Initial release.
