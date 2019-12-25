# ox-json
[![MELPA](https://melpa.org/packages/ox-json-badge.svg)](https://melpa.org/#/ox-json)
[![Build Status](https://travis-ci.org/jlumpe/ox-json.svg?branch=master)](https://travis-ci.org/jlumpe/ox-json)

JSON export back end for Emacs Org mode


See the [documentation file](ox-json-docs.org) for more detailed information.


## Usage

`(require 'ox-json')` somewhere and then use the `org-export-dispatch`
interactive command and select the J key for JSON export.

You can also use the `ox-json-export-to-buffer` and `ox-json-export-to-file`
functions or any of the built-in `org-export-` functions by passing `'json`
as the `backend` argument.


## Output

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

The `"contents"` property is an array containing the top-level elements of the document (which will
be an optional `section` element followed by any number of `headline` elements).

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

`"type"` is the node type returned by `org-element-type` and `"properties"` are property names and
values obtained from `org-element-property` (see documentation for the
[Org element API](https://orgmode.org/worg/dev/org-element-api.html) for a complete list of all node
types and properties). `ref` is a unique ID assigned to the node by Org mode's export code. Leading
colons in the property keys are omitted. `"contents"` is the encoded return value of
`org-element-contents`, the items of which are either more org nodes or strings.


### Nested data objects

The `"$$data-type"` property is added to JSON objects to indicate the type of structured data they
contain (the exception being generic sets of key/value pairs). See the documentation for more
information.


## Notes

The resulting JSON *should* include the correct choice of empty object (`{}`),
empty list (`[]`), `null`, or `false` for the given context, even though these are
represented as `nil` in elisp (don`t get me started).


## Related software

Check out the [pyorg](http://github.com/jlumpe/pyorg) Python package, which contains tools for
reading and manipulating the exported JSON data and interacting with a running Emacs process.
