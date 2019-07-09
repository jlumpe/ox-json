;;; ox-json.el --- JSON export backend for Org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jared Lumpe

;; Author: Jared Lumpe <mjlumpe@gmail.com>
;; Version: 0.1.0
;; Keywords: outlines
;; Homepage: https://github.com/jlumpe/ox-json

;; Package-Requires: ((emacs "25") (org "9") (s "1.12"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; TODO

;;; Code:

(eval-when-compile (require 'cl-macs))

(require 'cl-lib)
(require 's)
(require 'json)
(require 'ox)
(require 'org-element)


;;; Private constants

(defconst org-json--default-type-exporters
  `(
    bool             ,#'org-json-encode-bool
    string           ,#'org-json-encode-string
    number           ,#'org-json-encode-number
    node             ,#'org-json-export-property-node
    secondary-string ,#'org-json-export-secondary-string
    array            ,#'org-json-encode-array
    plist            ,#'org-json-encode-plist
    alist            ,#'org-json-encode-alist
    t                ,#'org-json-encode-auto))

(defconst org-json--default-property-types
  '(
    all (
      ; Never include parent, leads to infinite recursion
      :parent nil
      ; These properties have to do with absolute buffer positions and thus probably aren't useful to export
      :begin nil
      :end nil
      :contents-begin nil
      :contents-end nil
      ; These can be useful when converting from JSON to another format
      :post-affiliated number
      :pre-blank number
      :post-blank number)
    babel (
      :result (array string))
    entity (
      :latex-math-p bool
      :use-brackets-p bool)
    example-block (
      :preserve-indent bool
      :retain-labels bool
      :use-labels bool)
    headline (
      :archivedp bool
      :commentedp bool
      :deadline node
      :footnote-section-p bool
      :quotedp bool
      :scheduled node
      :tags (array string)
      :title secondary-string)
    inlinetask (
      :closed node
      :deadline node
      :scheduled node
      :title secondary-string)
    item (
      :structure nil
      :tag secondary-string)
    macro (
      :args (array string))
    plain-list (
      :structure array)
    planning (
      :closed node
      :deadline node
      :scheduled node)
    property-drawer (
      :properties nil) ; TODO
     ))


;;; Variables
(defgroup org-json nil "Customization for the ox-json package" :group 'outline)


;;; Utility code

(defun org-json--merge-alists (&rest alists)
  "Merge all alists in ALISTS, with keys in earlier alists overriding later ones."
  (cl-loop
    with keys = (make-hash-table :test 'equal)
    for alist in alists
    append
      (cl-loop
        for item in alist
        unless (gethash (car item) keys)
          collect item
        do (puthash (car item) t keys))))

(defun org-json--plists-get-default (key default &rest plists)
  "Try getting value for KEY from each plist in PLISTS in order, returning DEFAULT if not found."
  (cl-loop
    for plist in plists
    if (plist-member plist key)
      return (plist-get plist key)
    finally return default))

(defun org-json--plists-get (key &rest plists)
  "Try getting value for KEY from each plist in PLISTS in order, returning nil if not found."
  (apply #'org-json--plists-get-default key nil plists))

(cl-defmacro org-json--loop-plist ((key value plist) &body body)
  "Bind KEY and VALUE to each key-value pair in PLIST and execute BODY within a `cl-loop'."
  `(let ((--pl ,plist)
          (,key nil)
          (,value nil))
     (cl-loop
       while --pl
       do (setq
            ,key (car --pl)
            ,value (cadr --pl)
            --pl (cddr --pl))
       ,@body)))

(defun org-json--plist-to-alist (plist)
  "Convert plist PLIST to alist."
  (org-json--loop-plist (key value plist)
    collect (cons key value)))

(defun org-json-node-properties (node)
  "Get property plist of element/object NODE."
  ; It's the 2nd element of the list
  (cadr node))

(defun org-json--is-node (value)
  "Check if VALUE is an org element/object."
  (and
    (listp value)
    (listp (cdr value))
    (>= (length value) 2)
    (symbolp (car value))
    (listp (cadr value))))


;;; Define the backend

(org-export-define-backend 'json
  ;; Transcoders
  (org-json--merge-alists
    '(
      (template . org-json-transcode-template)
      (plain-text . org-json-transcode-plain-text)
      (headline . org-json-transcode-headline))
    ; Default for all remaining element/object types
    (cl-loop
      for type in (append org-element-all-elements org-element-all-objects)
      collect (cons type #'org-json-transcode-base)))
  ;; Filters
  :filters-alist '()
  ;; Options
  :options-alist
  '(
     (:json-data-type-property nil "json-data-type-property" "$$data_type")
     (:json-exporters nil nil nil)
     (:json-property-types nil nil nil)
     (:json-strict nil nil nil)
     (:json-include-extra-properties nil nil t)
     )
  ;; Menu
  :menu-entry
  '(?j "JSON" (
	(?J "As JSON buffer" org-json-export-as-json)
	(?j "To JSON file" org-json-export-to-json)))
  )


;;; User export functions

(defun org-json-export-as-json
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a JSON buffer."
  ; Modified from org-html-export-as-html:
  (interactive)
  (let ((buffer (org-export-to-buffer 'json "*Org JSON Export*"
                  async subtreep visible-only body-only ext-plist)))
    ; Switch to json mode if available
    (when (require 'json-mode nil t)
      (with-current-buffer buffer
        (json-mode)))
    buffer))


(defun org-json-export-to-json
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a JSON file."
  ; Modified from org-html-export-to-html:
  (interactive)
  (let ((file (org-export-output-file-name ".json" subtreep)))
    (org-export-to-file 'json file
      async subtreep visible-only body-only ext-plist)))


;;; Error handling

(defun org-json--make-error-obj (info msg args)
  "Create a JSON object with an error message."
  (let ((msg-encoded (json-encode-string (apply #'format msg args))))
    (org-json-encode-alist-raw "error" `((message . ,msg-encoded)) info)))

(defun org-json--error (info msg &rest args)
  "Either signal an error or return an encoded error object based off the :strict export setting."
  (if (plist-get info :json-strict)
    (apply #'error msg args)
    (org-json--make-error-obj info msg args)))

(cl-defun org-json--type-error (type value info &optional (maxlen 200))
  "Encode or signal an error when asked to encode a value that is not of the expected type."
  (cl-assert (stringp type))
  (let ((value-str (format "%S" value)))
    (when (> (length value-str) maxlen)
      (setq value-str
        (format "%s... (truncated printed value at %d characters)"
          (substring value-str 0 maxlen)
          maxlen)))
    (org-json--error info "Expected %s, got %s" type value-str)))


;;; Encoders for generic data types

(defun org-json-encode-bool (value &optional info)
  "Encode VALUE to JSON as boolean.

INFO is the plist of export options."
  (cond
    ((equal value t)
      "true")
    (value
      (org-json--type-error "boolean" value info))
    (t
      "false")))

(defun org-json-encode-string (value &optional info)
  "Encode VALUE to JSON as string or null.

INFO is the plist of export options.

Also accepts symbols."
  (cond
    ((not value)
      "null")
    ((stringp value)
      (json-encode-string value))
    ((symbolp value)
      (json-encode-string (symbol-name value)))
    (t
      (org-json--type-error "string or symbol" value info))))

(defun org-json-encode-number (value &optional info)
  "Encode VALUE to JSON as number or null.

INFO is the plist of export options."
  (cond
    ((numberp value)
      (json-encode-number value))
    ((not value)
      "null")
    (t
      (org-json--type-error "number" value info))))

(defun org-json-encode-array-raw (array &optional info)
  "Encode array to JSON given its already-encoded items.

ARRAY is a list of strings with encoded JSON data.
INFO is the plist of export options."
  (if array
    (format "[\n%s\n]" (s-join ",\n" array))
    "[]"))

(defun org-json-encode-alist-raw (data-type alist &optional info)
  "Encode alist ALIST containing pre-encoded values into JSON object.

DATA-TYPE is the data type string of the returned object.
INFO is the plist of export options"
  (let ((data-type-property (plist-get info :json-data-type-property)))
    (when (and data-type-property data-type)
      (push (cons data-type-property (json-encode-string data-type)) alist))
    (format "{\n%s\n}"
      (s-join ",\n"
        (cl-loop
          for (key . value) in alist
          collect (format "%s: %s" (json-encode-key key) (s-trim value)))))))

(defun org-json-encode-plist-raw (data-type plist &optional info)
  "Encode plist PLIST containing pre-encoded values into JSON object.

DATA-TYPE is the data type string of the returned object.
INFO is the plist of export options."
  (org-json-encode-alist-raw data-type (org-json--plist-to-alist plist) info))

(defun org-json-encode-auto (value &optional info)
  "Encode VALUE to JSON when its type is not known ahead of time.

INFO is the plist of export options.

Handles strings, numbers, and org elements/objects without a problem.
Non-empty lists which are not elements/objects are recursively encoded as
JSON arrays. Symbols are encoded as strings except for t which is encoded
as true. This function cannot tell whether a nil value should correspond to an
empty array, false, or null. A null value is arbitrarily returned in this case."
  (cond
    ((not value)
      "null")
    ((stringp value)
      (json-encode-string value))
    ((numberp value)
      (json-encode-number value))
    ((equal value t)
      "true")
    ((symbolp value)
      (json-encode-string (symbol-name value)))
    ((listp value)
      (if (org-json--is-node value)
        (org-json-export-data value info)
        (org-json-encode-array value info)))
    (t
      (org-json--error info "Don't know how to encode value %S"  value))))

(defun org-json--get-type-encoder (typekey &optional info)
  (org-json--plists-get typekey
    (plist-get info :json-exporters)
    org-json--default-type-exporters))

(defun org-json-encode-with-type (type value info)
  "Encode a VALUE to JSON given its type.

TYPE is a key in the plist under the :json-exporters option.
INFO is the plist of export options."
  (let* ((typekey (if (listp type) (car type) type))
         (args (if (listp type) (cdr type) nil))
         (encoder (org-json--get-type-encoder typekey info)))
    (if encoder
      (apply encoder value info args)
      (org-json--error info "Unknown type symbol %s" type))))

(cl-defun org-json-encode-array (array &optional info (itemtype t))
  "Encode the list ARRAY as a JSON array.

INFO is the plist of export options.
ITEMTYPE is optional and is the type to pass to `org-json-encode-with-type'
to encode the items of the array. By default `org-json-encode-auto' is used."
  (let ((encoder (org-json--get-type-encoder itemtype info)))
    (if encoder
      (org-json-encode-array-raw
        (cl-loop
          for item in array
          collect (funcall encoder item info))
        info)
      (org-json--error info "Unknown type symbol %s" itemtype))))

(cl-defun org-json-encode-alist (data-type alist &optional info (valuetype t))
  "Encode the alist ALIST as a JSON object.

DATA-TYPE is a data type string to add to the JSON object.
INFO is the plist of export options.
VALUETYPE is optional and is the type to pass to `org-json-encode-with-type'
to encode the values of each key-value pair. By default
`org-json-encode-auto' is used."
  (let ((encoder (org-json--get-type-encoder valuetype info)))
    (if encoder
      (org-json-encode-alist-raw
        data-type
        (cl-loop
          for (key . value) in alist
          collect (cons key (funcall encoder value info)))
        info)
      (org-json--error info "Unknown type symbol %s" valuetype))))

(cl-defun org-json-encode-plist (data-type plist &optional info (valuetype t))
  "Encode the plist PLIST as a JSON object.

DATA-TYPE is a data type string to add to the JSON object.
INFO is the plist of export options.
VALUETYPE is optional and is the type to pass to `org-json-encode-with-type'
to encode the values of each key-value pair. By default
`org-json-encode-auto' is used."
  (org-json-encode-alist
    data-type
    (org-json--plist-to-alist plist)
    info
    valuetype))


;;; Export generic org data

(defun org-json-export-data (data info)
  "Like `org-export-data' but properly format secondary strings as arrays.

DATA is an org element/object, string, or secondary string.
INFO is the plist of export options."
  (let ((exported (s-trim (org-export-data data info))))
    (if (and (listp data) (not (org-json--is-node data)))
      (format (if (> (length data) 1) "[\n%s\n]" "[%s]") exported)
      exported)))

(defun org-json-export-secondary-string (sstring info)
  "Export the secondary string SSTRING as a JSON array.

INFO is the plist of export options.

A secondary string is alist of org elements/objects and strings."
  (if (listp sstring)
    (org-json-encode-array-raw
      (mapcar
        (lambda (item)
          (cond
            ((stringp item)
              (json-encode-string item))
            ((org-json--is-node item)
              (org-json-export-property-node item info))
            (t
              (org-json--type-error info "org node or string" item))))
        sstring))
    (org-json--type-error info "list" sstring)))

(defun org-json-export-property-node (node info)
  "Export an object or element NODE that appears in a property of another node.

INFO is the plist of export options.

Interprets nil as null."
  (cond
    ((org-json--is-node node)
      (org-json-export-data node info))
    ((not node)
      "null")
    (t
      (org-json--type-error "org node or nil" node info))))

(defun org-json-export-contents (node info)
  "Export the contents of org element/object NODE as a JSON array.

INFO is the plist of export options.

This is used in place of the \"contents\" argument passed to the transcoder
functions in order to control how the transcoded values of each child node
are joined together, which apparently cannot be overridden. This shouldn't
result in too much extra work being done because the exported value of each
node is memoized."
  (cl-assert (org-json--is-node node))
  (org-json-encode-array-raw
    (cl-loop
      with encoded-items = nil
      for item in (org-element-contents node)
      do (let ((encoded (org-json-export-data item info)))
           (unless (s-blank? encoded)
             (push encoded encoded-items)))
      finally return (nreverse encoded-items))))

(defun org-json-get-property-type (node-type property info)
  "Get the type of a property in elements/objects of a given type.

NODE-TYPE is a symbol returned by `org-element-type', or 'all to
get the default value.
PROPERTY is the property key to be used with `org-element-property'
\(should start with colon).
INFO is the plist of export options.

The type is looked up from the :json-property-types option, first
looking up the type-specific plist of property types using
NODE-TYPE and then defaulting to the \"all\" type if not found.
The type symbol is then looked up in this plist using PROPERTY.
Returns a symbol which can be passed to `org-json-encode-with-type'."
  (let ((info-types (plist-get info :json-property-types))
        (include-extra (plist-get info :json-include-extra-properties)))
    (org-json--plists-get-default
      property
      (if include-extra t nil)
      (plist-get info-types node-type)
      (plist-get info-types 'all)
      (plist-get org-json--default-property-types node-type)
      (plist-get org-json--default-property-types 'all))))

(defun org-json-export-properties-alist (node info)
  "Get alist of encoded property values for element/object NODE.

INFO is the plist of export options.

Returns an alist where the items are property names and their
JSON-encoded values."
  (let ((node-type (org-element-type node))
        (property-type nil))
    (org-json--loop-plist (key value (org-json-node-properties node))
      with items = nil
      do (setq property-type (org-json-get-property-type node-type key info))
      if property-type
        collect (cons key (org-json-encode-with-type property-type value info)))))

(cl-defun org-json-export-node-base (node info &key extra
                                      (properties (org-json-export-properties-alist node info))
                                      (contents (org-json-export-contents node info)))
  "Base export function for a generic org element/object.

NODE is an org element or object and INFO is the export environment plist.
INFO is the plist of export options.
PROPERTIES is an alist of pre-encoded property values that will be used in place
of the return value of `org-json-export-properties-alist' if given (passing a
value of nil will result in no properties being included).
EXTRA is an alist of keys and pre-encoded values to add directly to the returned
JSON object at the top level (note that this is not checked for conflicts with
the existing keys).
CONTENTS overrides the default way of encoding the node's contents with
`org-json-export-node-contents'. It can either be a string containing the entire
encoded JSON array or a list of pre-encoded strings.

It is expected for all transcoding functions to call this function to do most
of the work, possibly using the keyword arguments to override behavior."
  (unless (stringp contents)
    (setq contents (org-json-encode-array-raw contents)))
  (org-json-encode-alist-raw
    "org-node"
    `(
      (type . ,(json-encode-string (symbol-name (org-element-type node))))
      ,@extra
      (properties . ,(org-json-encode-alist-raw "mapping" properties info))
      (contents . ,contents))
    info))


;;; Transcoder functions

(defun org-json-transcode-plain-text (text info)
  "Transcode plain text to a JSON string.

TEXT is a string to encode.
INFO is the plist of export options."
  ; Ignore empty strings
  (unless (string= text "")
    (json-encode-string text)))

(cl-defun org-json-transcode-base (node contents info)
  "Default transcoding function for all element/object types.

NODE is an element or object to encode.
CONTENTS is a string containing the encoded contents of the node,
but its value is ignored (`org-json-export-contents' is used instead).
INFO is the plist of export options."
  (org-json-export-node-base node info))

(defun org-json--get-doc-info-alist (info)
  "Get alist of top-level document properties (values already encoded)."
  `(
     (title . ,(org-json-export-secondary-string (plist-get info :title) info))
     (file_tags . ,(json-encode-list (plist-get info :filetags)))
     (author . ,(org-json-export-secondary-string (plist-get info :author) info))
     (creator . ,(org-json-encode-string (plist-get info :creator) info))
     (date . ,(org-json-export-secondary-string (plist-get info :date) info))
     (description . ,(org-json-encode-string (plist-get info :description) info))
     (email . ,(org-json-encode-string (plist-get info :email) info))
     (language . ,(org-json-encode-string (plist-get info :language) info))
     ))

(defun org-json-transcode-template (contents info)
  "Transcode an entire org document to JSON.

CONTENTS is a string containing the encoded document contents,
but its value is ignored (`org-json-export-contents' is used instead).
INFO is the plist of export options."
  (let* ((docinfo (org-json--get-doc-info-alist info))
         (parse-tree (plist-get info :parse-tree))
         (contents-encoded (org-json-export-contents parse-tree info)))
    (org-json-encode-alist-raw
      "org-document"
      `(
         ,@docinfo
         (contents . ,contents-encoded))
      info)))

(defun org-json-transcode-headline (headline contents info)
  "Transcode a headline element to JSON.

HEADLINE is the parsed headline to encode.
CONTENTS is a string containing the encoded contents of the headline,
but its value is ignored (`org-json-export-contents' is used instead).
INFO is the plist of export options."
  (org-json-export-node-base headline info
    :extra `(
       (ref . ,(json-encode-string (org-export-get-reference headline info))))))


(provide 'ox-json)

;;; ox-json.el ends here
