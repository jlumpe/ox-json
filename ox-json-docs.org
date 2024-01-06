#+title: ox-json documentation
#+author: Jared Lumpe


* TODO Installation

* TODO Usage

~ox-json-export-to-file~
~ox-json-export-to-buffer~

* Output format

** Data types

Exported JSON objects which contain structured data have a special "data type" property which
indicates the type of data they represent. This is meant to make it easier for external tools to
interpret. Objects lacking this property represent simple key-value mappings with no additional
implied structure.

By default this property is named ~"$$data_type"~ but its value can be
customized with the =:json-data-type-property= option (see the [[*Options][options]] section).

*** org-document
This is the top-level object generated when the entire file is exported.

**** Properties
- ~"properties"~ (object) :: File-level properties
  - ~"title"~ (array) :: Document title (secondary string)
  - ~"filetags"~ (array of strings) ::
  - ~"author"~ (string) ::
  - ~"creator"~ (string) ::
  - ~"date"~ (array) :: (secondary string)
  - ~"description"~ (array) :: (secondary string)
  - ~"email"~ (string) ::
  - ~"language"~ (string) ::
- ~"contents"~ (array) :: The top-level elements in the document. This should
     consist of an optional =section= node followed by any number of =header= nodes.

*** org-node
A node (element or object) in the document's parse tree.

See the [[https://orgmode.org/worg/dev/org-element-api.html][Org element API documentation]] for the list of node types and their properties.

**** Properties
- ~"type"~ (string) :: Type of node, given by ~org-element-type~.
- ~"ref"~ (string) :: A unique string assigned to all nodes during the export process, from
                    ~org-export-get-reference~.
- ~"contents"~ (array) :: Contents of the node. Array of node objects or strings.
- ~"properties"~ (object) :: Object/element properties from org element API.
- ~"drawer"~ (object) :: (headline nodes only) Properties defined in ~:PROPERTIES:~ drawer.

*** error
This object appears in place of other values (typically within an =org-node='s properties) when
there was an error exporting that value.

- ~"message"~ (string) :: A message describing why the error occurred.


** Node types with specific handling
Some node types are exported with additional data:

*** headline
*** link
**** Additional properties
- =is-inline-image= (bool) :: Whether org mode think this link should be rendered as an inline
  image.
- =is-internal= (bool) :: Whether this link points to a location in the same document. True for the
     =id=, =fuzzy=, and =radio= link  types.
- =target-ref= (string) :: The =ref= property of the headline or other node this link resolves
  to. Only exists for internal links.
*** timestamp
**** Additional properties
- ~"start"~, ~"end"~ (string) :: Year/month/day/hour/minute from the =start-*= and =end-*= sets of
  properties combined together and encoded as a single string in
  [[https://www.w3.org/TR/NOTE-datetime][ISO 8601]] format.

* TODO Agenda

* Customization
** Options

| Property                         | Data type | Default                                        |
|----------------------------------+-----------+------------------------------------------------|
| =:json-data-type-property=       | string    | ~"$$data_type"~                                |
| =:json-exporters=                | plist     | See variable ~ox-json-default-type-exporters~ |
| =:json-property-types=           | plist     | See variable ~ox-json-default-property-types~ |
| =:json-strict=                   | bool      | ~nil~                                          |
| =:json-include-extra-properties= | bool      | ~t~                                            |

*** Details
- =:json-data-type-property= ::
  Name of the property added to JSON objects which indicates the type of data they represent.
  Set to ~nil~ to disable.
- =:json-exporters= ::
- =:json-property-types= ::
- =:json-strict= ::
- =:json-include-extra-properties= ::

* Changelog

** 0.3.0
*** Backwards-incompatible changes
- All of the top-level document attributes (=author=, =date=, etc.) are now grouped under the
  ~"properties"~ key of the =org-document= object.
- Export =start= property of =timestamp= nodes with the correct name. Was being exported with the
  name =begin=.
- JSON objects representing generic key/value mappings no longer have the ~"$$data_type"~ property.

*** Features
- Data in property drawers is now exported with headlines in the ~"drawer"~ property.

*** Other
- Now requires Emacs 26.1.
- Reorganized/cleaned up much of the internals.

** 0.2.0

** 0.1
Initial version.

* Internals
** TODO API
*** Basic JSON encoding functions
*** Advanced JSON encoding
*** Exporting org nodes and properties
