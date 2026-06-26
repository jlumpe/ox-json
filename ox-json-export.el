;;; ox-json-export.el --- Org export and transcoding for ox-json  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 - 2023 Jared Lumpe

;; Author: Jared Lumpe <jared@jaredlumpe.com>

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

;; Org document export, transcoder functions, and output filters for ox-json.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'cl-lib)
(require 's)
(require 'json)
(require 'ox)
(require 'org-element)
(require 'ox-json-core)
(require 'ox-json-utils)
(require 'ox-json-encode)


;;; Export generic org data

(defun ox-json-export-data (data info)
  "Like `org-export-data' but properly format secondary strings as arrays.

DATA is an org element/object, string, or secondary string.
INFO is the plist of export options."
  (let ((exported (s-trim (org-export-data data info))))
    (if (and (listp data) (not (ox-json--is-node data)))
      (format (if (> (length data) 1) "[\n%s\n]" "[%s]") exported)
      exported)))

(defun ox-json-encode-tag-string (string info)
  "Encode an un-split tag string as a JSON array.

STRING is a collection of tags joined by colon characters.
INFO is the plist of export options."
  (if string
    (ox-json-encode-array (s-split ":" string t) info 'string t)
    "[]"))

(defun ox-json-export-secondary-string (sstring info)
  "Export the secondary string SSTRING as a JSON array.

INFO is the plist of export options.

A secondary string is alist of org elements/objects and strings."
  (if (listp sstring)
    (ox-json-encode-array-raw
      (mapcar
        (lambda (item)
          (cond
            ((stringp item)
              (json-encode-string item))
            ((ox-json--is-node item)
              (ox-json-export-property-node item info))
            (t
              (ox-json--type-error "org node or string" item info))))
        sstring))
    (ox-json--type-error "list" sstring info)))

(defun ox-json-export-property-node (node info)
  "Export an object or element NODE that appears in a property of another node.

INFO is the plist of export options.

Interprets nil as null."
  (cond
    ((ox-json--is-node node)
      (ox-json-export-data node info))
    ((not node)
      "null")
    (t
      (ox-json--type-error "org node or nil" node info))))

(defun ox-json-export-timestamp-property (timestamp info)
  "Export a timestamp object that appears in the properties of another element.

TIMESTAMP is the timestamp object from the org buffer parse tree.
INFO is the plist of export options."
  (ox-json-make-object "timestamp" info
    `(
      (start string ,(ox-json-timestamp-isoformat timestamp "start" info))
      (end string ,(ox-json-timestamp-isoformat timestamp "end" info))
      (type string ,(org-element-property :type timestamp))
      (raw-value string ,(org-element-property :raw-value timestamp))
      (repeater nil
        ,(ox-json-make-object nil info
           `(
              (type string ,(org-element-property :repeater-type timestamp))
              (unit string ,(org-element-property :repeater-unit timestamp))
              (value number ,(org-element-property :repeater-value timestamp)))))
       (warning nil
         ,(ox-json-make-object nil info
            `(
               (type string ,(org-element-property :warning-type timestamp))
               (unit string ,(org-element-property :warning-unit timestamp))
               (value number ,(org-element-property :warning-value timestamp))))))))

(defun ox-json-export-contents (node info)
  "Export the contents of org element/object NODE as a JSON array.

INFO is the plist of export options.

This is used in place of the \"contents\" argument passed to the transcoder
functions in order to control how the transcoded values of each child node
are joined together, which apparently cannot be overridden. This shouldn't
result in too much extra work being done because the exported value of each
node is memoized."
  (cl-assert (ox-json--is-node node))
  (ox-json-encode-array-raw
    (cl-loop
      with encoded-items = nil
      for item in (org-element-contents node)
      do (let ((encoded (ox-json-export-data item info)))
           (unless (s-blank? encoded)
             (push encoded encoded-items)))
      finally return (nreverse encoded-items))))

(defun ox-json--get-property-types (node-type info)
  "Get property type plists for a given node type.

NODE-TYPE is the symbol returned by `org-element-type'.
INFO is the plist of export options."
  (let ((info-types (plist-get info :json-property-types)))
    (list
      (plist-get info-types node-type)
      (plist-get info-types 'all)
      (plist-get ox-json-default-property-types node-type)
      (plist-get ox-json-default-property-types 'all))))

(defun ox-json--get-property-defaults (node-type)
  "Get property default value plists for NODE-TYPE in priority order."
  (list
    (plist-get ox-json-default-property-values node-type)
    (plist-get ox-json-default-property-values 'all)))

(defun ox-json--is-default-property-value (key value node-type)
  "Return non-nil if VALUE is the configured default for KEY in NODE-TYPE."
  (let* ((default-plists (ox-json--get-property-defaults node-type))
         (default-val (apply #'ox-json--plists-get-default
                        key ox-json--absent default-plists)))
    (and (not (eq default-val ox-json--absent))
         (equal value default-val))))

(defun ox-json--remove-default-properties (property-plist node-type)
  "Return a copy of PROPERTY-PLIST with default-valued properties removed."
  (let ((result nil))
    (ox-json--loop-plist (key value property-plist)
      do (unless (ox-json--is-default-property-value key value node-type)
           (setq result (append result (list key value)))))
    result))

(defun ox-json-export-properties (node info &optional property-types)
  "Get alist of encoded property values for element/object NODE.

INFO is the plist of export options.
PROPERTY-TYPES is an optional additional plist of property type symbols that
overrides the defaults for the type of NODE.

Returns an alist where the items are property names and their
JSON-encoded values."
  (let ((node-type (org-element-type node))
        (property-plist (ox-json-node-properties node)))
    (ox-json--export-properties-for-type node-type property-plist info property-types)))

(defun ox-json--export-properties-for-type (node-type property-plist info &optional property-types)
  "Export a plist of properties for the given element/object type.

NODE-TYPE is the symbol returned by `org-element-type'.
PROPERTY-PLIST is a plist containing the property values.
INFO is the plist of export options.
PROPERTY-TYPES is an optional additional plist of property type symbols that
overrides the defaults derived from INFO and NODE-TYPE.

Returns an alist where the items are property names and their
JSON-encoded values."
  (let ((type-plists (ox-json--get-property-types node-type info))
        (include-extra (plist-get info :json-include-extra-properties))
        (omit-defaults (plist-get info :json-omit-default-property-values)))
    (when property-types
      (push property-types type-plists))
    (when omit-defaults
      (setq property-plist
        (ox-json--remove-default-properties property-plist node-type)))
    (apply #'ox-json--export-properties-base
      property-plist
      (if include-extra t nil)
      info
      type-plists)))

(defun ox-json--skip-property (property)
  "Return non-nil if an element property PROPERTY should be skipped, regardless of the value of the
:json-property-types option."
  ; Apparently 9.7 introduces some private property names, skip these.
  (cl-search "--" (symbol-name property)))

(defun ox-json--export-properties-base (property-plist default-type info &rest type-plists)
  "Export org node property values by looking up their types in a series of plists.

PROPERTY-PLIST is a plist containing the property values.
DEFAULT-TYPE is the type symbol to be used for properties not found in
TYPE-PLISTS. A value of nil means these properties will be ignored.
INFO is the plist of export options.
TYPE-PLISTS is a sequence of plists containing the type symbols used to encode
property values with (see `ox-json-encode-with-type'). The lookup stops at the
first match, so earlier plists override later ones.

Returns an alist where the items are property names and their
JSON-encoded values."
  (let ((property-type nil))
    (ox-json--loop-plist (key value property-plist)
      do (setq property-type
           (apply #'ox-json--plists-get-default key default-type type-plists))
      if (and property-type (not (ox-json--skip-property key)))
        collect (cons key (ox-json-encode-with-type property-type value info)))))

(defun ox-json--get-reference (datum info)
  "Return a stable or random reference for DATUM, depending on options.
When `:json-deterministic-refs' is non-nil, the ref is derived from
DATUM's structural path in the parse tree.  Otherwise delegates to
`org-export-get-reference' (random)."
  (if (plist-get info :json-deterministic-refs)
      (let ((cache (plist-get info :internal-references)))
        (or (car (rassq datum cache))
            (let ((ref (ox-json--format-structural-ref datum)))
              (plist-put info :internal-references
                         (cons (cons ref datum) cache))
              ref)))
    (org-export-get-reference datum info)))

(cl-defun ox-json-export-node-base
  (node info &key
    property-types
    (ref (ox-json--get-reference node info))
    (properties (ox-json-export-properties node info property-types))
    extra-properties
    extra
    (contents (ox-json-export-contents node info)))
  "Base export function for a generic org element/object.

NODE is an org element or object.
INFO is the plist of export options.
PROPERTY-TYPES is a plist of type symbols which override the default way of
determining how to encode property values (see equivalent argument in
`ox-json-export-properties').
PROPERTIES is an alist of pre-encoded property values that will be used in place
of the return value of `ox-json-export-properties' if given (passing a
value of nil will result in no properties being included).
EXTRA-PROPERTIES is an alist of pre-encoded property values to add to the
automatically-derived ones instead of replacing them, as the PROPERTIES argument
does.
EXTRA is an alist of keys and pre-encoded values to add directly to the returned
JSON object at the top level (note that this is not checked for conflicts with
the existing keys).
CONTENTS overrides the default way of encoding the node's contents with
`ox-json-export-node-contents'. It can either be a string containing the entire
encoded JSON array or a list of pre-encoded strings.

It is expected for all transcoding functions to call this function to do most
of the work, possibly using the keyword arguments to override behavior."
  (unless (stringp contents)
    (setq contents (ox-json-encode-array-raw contents)))
  (when extra-properties
    (setq properties (append properties extra-properties)))
  (when properties
    (setq properties (ox-json--sort-alist-by-key properties)))
  (ox-json-encode-alist-raw
    "org-node"
    (let ((base `(
        (type . ,(json-encode-string (symbol-name (org-element-type node))))
        (ref . ,(json-encode-string ref))
        ,@extra
        (properties . ,(ox-json-encode-alist-raw nil properties info)))))
      (if (string= contents "[]")
          base
        (append base (list (cons 'contents contents)))))
    info))


;;; Transcoder functions

(defun ox-json-transcode-plain-text (text &optional _info)
  "Transcode plain text to a JSON string.

TEXT is a string to encode.
INFO is the plist of export options."
  ; Ignore empty strings
  (unless (string= text "")
    (json-encode-string text)))

(defun ox-json-transcode-base (node _contents info)
  "Default transcoding function for all element/object types.

NODE is an element or object to encode.
CONTENTS is a string containing the encoded contents of the node,
but its value is ignored (`ox-json-export-contents' is used instead).
INFO is the plist of export options."
  (let ((ox-json--current-node node))
    (ox-json-export-node-base node info)))

(defun ox-json-document-properties (info)
  "Get alist of top level document properties (values already encoded).

INFO is the plist of export options."
  (ox-json-make-alist
    info
    `(
      (title secondary-string ,(plist-get info :title))
      (filetags (array string) ,(plist-get info :filetags))
      (author secondary-string ,(plist-get info :author))
      (creator string ,(plist-get info :creator))
      (date secondary-string ,(plist-get info :date))
      (description secondary-string ,(plist-get info :description))
      (email string ,(plist-get info :email))
      (language string ,(plist-get info :language)))))

(defun ox-json-transcode-template (_contents info)
  "Transcode an entire org document to JSON.

CONTENTS is a string containing the encoded document contents,
but its value is ignored (`ox-json-export-contents' is used instead).
INFO is the plist of export options."
  (let* ((ox-json--current-node "<document root>")
         (properties (ox-json--sort-alist-by-key (ox-json-document-properties info)))
         (properties-encoded (ox-json-encode-alist-raw nil properties info))
         (parse-tree (plist-get info :parse-tree))
         (drawer-properties (ox-json-document-drawer-properties parse-tree info))
         (drawer-encoded (when drawer-properties
                           (ox-json-encode-plist nil drawer-properties info 'string)))
         (contents-encoded (ox-json-export-contents parse-tree info)))
    (ox-json-encode-alist-raw
      "org-document"
      (let ((base `((properties . ,properties-encoded)
                    ,@(when drawer-encoded
                        `((drawer . ,drawer-encoded))))))
        (if (string= contents-encoded "[]")
            base
          (append base (list (cons 'contents contents-encoded)))))
      info)))

(cl-defun ox-json-transcode-headline (headline _contents info &rest kw &key extra)
  "Transcode a headline element to JSON.

HEADLINE is the parsed headline to encode.
CONTENTS is a string containing the encoded contents of the headline,
but its value is ignored (`ox-json-export-contents' is used instead).
INFO is the plist of export options.
KW is a plist of keyword arguments to pass to `ox-json-export-node-base'.
EXTRA is an alist of additional properties to attach to the exported JSON object
at the top level."
  (pcase-let*
    ((ox-json--current-node headline)
     (all-props (ox-json-node-properties headline))
     (`(,regular-props . ,drawer-props)
       (ox-json--separate-drawer-properties all-props info))
     (props-encoded
       (ox-json--export-properties-for-type 'headline regular-props info))
     (drawer-encoded
       (ox-json-encode-plist nil drawer-props info 'string)))
    ; Add "drawer" object to top-level JSON properties
    (push
      (cons "drawer" drawer-encoded)
      extra)
    ; Add list of tags including inherited
    (push
      (cons 'tags-all
        (ox-json-encode-array (ox-json-headline-tags-all headline info) info 'string))
      extra)
    (apply #'ox-json-export-node-base headline info
      :properties props-encoded
      :extra extra
      kw)))

(defun ox-json-link-extra-properties (link info)
  "Get properties to export from a link object.

LINK is the parsed link object.
INFO is the plist of export options."
  (let* ((link-type (intern (org-element-property :type link)))
         (is-internal nil)
         (target-ref nil))
    (when (memq link-type '(custom-id fuzzy radio))
      (setq is-internal t)
      (let* ((target
              ; At least one of these functions throws an error if it doesn't resolve
              (condition-case nil
                (cl-case link-type
                  (custom-id
                    (org-export-resolve-id-link link info))
                  (fuzzy
                    (org-export-resolve-fuzzy-link link info))
                  (radio
                    (org-export-resolve-radio-link link info)))
                ; TODO: handle more specific error type?
                (error nil))))
        (when target
          (setq target-ref (ox-json--get-reference target info)))))
    (ox-json-make-alist
      info
      `(
        (is-internal bool ,is-internal)
        (target-ref string ,target-ref)
        (is-inline-image bool ,(org-export-inline-image-p link))))))

(defun ox-json-transcode-link (link _contents info)
  "Transcode a link object to JSON.

LINK is the parsed link to transcode.
CONTENTS is a string containing the encoded contents of the element,
but its value is ignored (`ox-json-export-contents' is used instead).
INFO is the plist of export options."
  (let ((ox-json--current-node link))
    (ox-json-export-node-base link info
      :extra (ox-json-link-extra-properties link info))))

(defun ox-json-timestamp-extra-properties (timestamp info)
  "Get additional properties to export from a timestamp object.

TIMESTAMP is the parsed timestamp object.
INFO is the plist of export options."
  (ox-json-make-alist
    info
    `(
      (start string ,(ox-json-timestamp-isoformat timestamp "start" info))
      (end string ,(ox-json-timestamp-isoformat timestamp "end" info)))))

(defun ox-json-transcode-timestamp (timestamp _contents info)
  "Transcode a timestamp object to JSON.

TIMESTAMP is the parsed link to transcode.
CONTENTS is a string containing the encoded contents of the element,
but its value is ignored (`ox-json-export-contents' is used instead).
INFO is the plist of export options."
  (let ((ox-json--current-node timestamp))
    (ox-json-export-node-base timestamp info
      :extra (ox-json-timestamp-extra-properties timestamp info))))


;;; Filter functions

(defun ox-json-filter-final-output (text back-end info)
  "Post-process the entire output.

TEXT is the full exported text string.
BACK-END is the export back-end symbol.
INFO is the plist of export options."
  (let ((postprocess (plist-get info :json-postprocess)))
    (cond
      ; Not JSON?
      ((not (eq back-end 'json))
        text)
      ; Post-process
      ((memq postprocess '(pretty minimal))
        ; Is this always available?
        (if (fboundp 'json-pretty-print-buffer)
          (with-temp-buffer
            (insert text)
            (json-pretty-print-buffer (eq postprocess 'minimal))
            (buffer-string))
          (warn "Unable to post-process JSON, json-pretty-print-buffer not available.")
          text))
      ; No post-processing
      ((not postprocess)
        text)
      ; Invalid
      (t
        (warn "Invalid value for :json-postprocess option: %S" postprocess)
        text)
    )))


(provide 'ox-json-export)

;;; ox-json-export.el ends here
