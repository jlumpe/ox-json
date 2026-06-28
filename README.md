# ox-json
[![MELPA](https://melpa.org/packages/ox-json-badge.svg)](https://melpa.org/#/ox-json)
[![Build Status](https://github.com/jlumpe/ox-json/actions/workflows/ci.yml/badge.svg)](https://github.com/jlumpe/ox-json/actions/workflows/ci.yml)

JSON export back end for Emacs Org mode.


## Usage

`(require 'ox-json)` somewhere and then use the `org-export-dispatch` interactive command and select
the J key for JSON export.

You can also use the `ox-json-export-to-buffer` and `ox-json-export-to-file` functions or any of the
built-in `org-export-` functions by passing `'json` as the `backend` argument.


## Output

### Document

An exported document looks like:

```json
{
  "$$data_type": "org-document",
  "properties": {
    "title": ["Test file"],
    "file_tags": ["foo","bar"],
    "author": ["jlumpe"],
    "creator": "Emacs 26.2 (Org mode 9.2.4)",
    "date": [],
    "description": ["Test file"],
    "email": "jlumpe@*****.org",
    "language": "en"
  },
  "contents": [
    ...
  ]
}
```

- `"contents"`: array containing the top-level elements of the document (which will be an optional
  `section` element followed by any number of `headline` elements).
- `"properties"`: File-level properties:
  - `"title"` (array, secondary string)
  - `"filetags"` (array of strings)
  - `"author"` (string)
  - `"creator"` (string)
  - `"date"` (array, secondary string)
  - `"description"` (array, secondary string)
  - `"email"` (string)
  - `"language"` (string)


### Nodes

All nodes (elements and objects) in the document tree are exported like:

```json
{
  "$$data_type": "org-node",
  "type": "headline",
  "ref": "orgd51ec19",
  "properties": { ... },
  "contents": [ ... ]
}
```

- `"type"` (string): node type returned by `org-element-type`
- `"properties"` (object): property names and values obtained from `org-element-property`. Leading
  colons in the keys are omitted.
- `ref` (string): unique string assigned to all nodes during the export process, from
  `org-export-get-reference`.
- `"contents"`: encoded return value of `org-element-contents`. Items are either more org nodes or
  strings.

See the [Org element API](https://orgmode.org/worg/dev/org-element-api.html) documentation for a
complete list of all node types and properties. Also see the section
[Node types with specific handling](node-types-with-specific-handling).


### Additional notes

The `"$$data-type"` property is added to JSON objects to indicate the type of structured data they
contain. This is meant to make it easier for external tools to interpret. Objects lacking this
property represent simple key-value mappings with no additional implied structure.

The resulting JSON *should* include the correct choice of empty object (`{}`),
empty list (`[]`), `null`, or `false` for the given context, even though these are
represented as `nil` in elisp (don`t get me started).


## Node types with specific handling

Some specific node types have additional properties added. These are at the outer level instead of
under the `"properties"` key to distinguish them from Org's builtin properties.

### headline

Additional properties:

- `"drawer"` (object): Properties defined in `:PROPERTIES:` drawer.
- `"all-tags"` (array of strings): All tags including those inherited from parents.


### link

Additional properties:

- `is-inline-image` (bool): Whether org mode think this link should be rendered as an inline
  image.
- `is-internal` (bool): Whether this link points to a location in the same document. True for the
  `id`, `fuzzy`, and `radio` link  types.
- `target-ref` (string): The `ref` property of the headline or other node this link resolves
  to. Only exists for internal links.


### timestamp

Additional properties:

- `"start"`, `"end"` (string): Year/month/day/hour/minute from the `start-*` and `end-*` sets of
  properties combined together and encoded as a single string in
  [ISO 8601](https://www.w3.org/TR/NOTE-datetime) format.


## Options

| Property                              | Data type | Default                                        |
|---------------------------------------|-----------|------------------------------------------------|
| `:json-data-type-property`            | string    | `"$$data_type"`                                |
| `:json-exporters`                     | plist     | See variable `ox-json-default-type-exporters`  |
| `:json-property-types`                | plist     | See variable `ox-json-default-property-types`  |
| `:json-strict`                        | bool      | `nil`                                          |
| `:json-include-extra-properties`      | bool      | `t`                                            |
| `:json-omit-default-property-values`  | bool      | `t`                                            |

- `:json-data-type-property`: Name of the property added to JSON objects which indicates the type of
  data they represent. Set to `nil` to disable.
- `:json-omit-default-property-values`: When non-nil, node properties whose value equals the
  configured default are omitted from export. Defaults are defined in
  `ox-json-default-property-values` (not customizable currently).


## Related software

Check out the [pyorg](http://github.com/jlumpe/pyorg) Python package, which contains tools for
reading and manipulating the exported JSON data and interacting with a running Emacs process.
