# ox-json
[![Build Status](https://travis-ci.org/jlumpe/ox-json.svg?branch=master)](https://travis-ci.org/jlumpe/ox-json)

JSON export back end for Emacs Org mode


## Usage

`(require 'ox-json')` somewhere and then use the `org-export-dispatch`
interactive command and select the J key for JSON export.

You can also use the `org-json-export-as-json` and `org-json-export-to-json`
functions or any of the built-in `org-export-` functions with `'json`
for the backend argument.


## Output

The output looks like:

```
{
  "$$data_type": "org-node",
  "type": "headline",
  "properties": { ... },
  "contents": [ ... ]
}
```

`"type"` is the node type returned by `org-element-type` and `"properties"` are property names and
values obtained from `org-element-property` (see documentation for the
[Org element API](https://orgmode.org/worg/dev/org-element-api.html) for a complete list of all node
types and properties). Leading colons in the property keys are omitted. `"contents"` is the encoded
return value of `org-element-contents`, the items of which are either more org nodes or strings.

The top-level data structure looks like:

```
{
  "$$data_type": "org-document",
  "title": ["Test file"],
  "file_tags": ["foo","bar"],
  "author": ["jlumpe"],
  "creator": "Emacs 26.2 (Org mode 9.2.4)",
  "date": [],
  "description": ["Test file"],
  "email": "jlumpe@*****.org",
  "language": "en",
  "contents": [
    ...
  ]
}
```

The `title`, `author`, `date`, and `description` properties are secondary strings (lists of strings
and possibly markup objects) which is why they are encoded as arrays.


### Nested data objects

The `"$$data-type"` property is added to differentiate encoded org elements/objects and other data
types from generic sets of key/value pairs that occur in alists or plists (the latter of which has
`"$$data-type": "mapping"`).

Additional data types are:

```
{
  "$$data-type": "error",
  "message": "Describes an error in automatically encoding this data structure."
}
```


## Notes

The resulting JSON *should* include the correct choice of empty object (`{}`),
empty list (`[]`), `null`, or `false` for the given context, even though these are
given a value of `nil` in elisp (don`t get me started).


## Configuration

TODO
