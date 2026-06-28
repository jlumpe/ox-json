;;; ox-json-encode.el --- JSON encoding for ox-json  -*- lexical-binding: t; -*-

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

;; Error handling and JSON encoding functions for ox-json.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'cl-lib)
(require 's)
(require 'json)
(require 'org-element)
(require 'ox-json-core)
(require 'ox-json-utils)

(declare-function ox-json-export-data "ox-json-export")


;;; Error handling

(defun ox-json--make-error-obj (info msg args)
  "Create a JSON object with an error message.

INFO is the plist of export options.
MSG is the error message.
ARGS are objects to insert into MSG using `format-message'."
  (ox-json-make-object "error" info
    `((message string ,(apply #'format-message msg args)))))

(defun ox-json--signal-error (msg &rest args)
  "Signal an error, adding information on the current element being transcoded if applicable.

MSG is the error message.
ARGS are objects to insert into MSG using `format-message'."
  (let* ((msg-formatted (apply #'format-message msg args))
         (node-type (org-element-type ox-json--current-node))
         (context-string))
    (setq context-string
      (cond
        ; Is an actual org node - give type and location
        (node-type
          (format "%s element on line %s"
            node-type
            (line-number-at-pos (org-element-property :begin ox-json--current-node))
          ))
        ; A string - use directly for context
        ((stringp ox-json--current-node)
          ox-json--current-node)
        ; Invalid?
        (ox-json--current-node
          "<unknown>")
        ; Otherwise nil
      ))
    (when context-string
      (setq msg-formatted (format "Error transcoding %s: %s" context-string msg-formatted)))
    (error msg-formatted)))

(defun ox-json--error (info msg &rest args)
  "Either signal an error or return an encoded error object.

Behavior is based off the :json-strict export setting.

INFO is the plist of export options.
MSG is the error message.
ARGS are objects to insert into MSG using `format'."
  (if (plist-get info :json-strict)
    (apply #'ox-json--signal-error msg args)
    (ox-json--make-error-obj info msg args)))

(cl-defun ox-json--type-error (type value info &optional (maxlen 200))
  "Encode or signal an error when asked to encode a value to an incompatible type.

TYPE is the argument to `ox-json-encode-with-type'.
VALUE is the value to be encoded.
INFO is the plist of export options.
MAXLEN is the number of characters to truncate the representation of VALUE at."
  (cl-assert (stringp type))
  (let ((value-str (format "%S" value)))
    (when (> (length value-str) maxlen)
      (setq value-str
        (format "%s... (truncated printed value at %d characters)"
          (substring value-str 0 maxlen)
          maxlen)))
    (ox-json--error info "Expected %s, got %s" type value-str)))


;;; Encoders for generic data types

(defun ox-json-encode-bool (value &optional info strict)
  "Encode VALUE to JSON as boolean.

INFO is the plist of export options.
If STRICT is true will only accept t as a true value and raise/return
an error otherwise. If false will accept any truthy value."
  (cond
    ((not value)
      "false")
    ((or (equal value t) (not strict))
      "true")
    (t
      (ox-json--type-error "boolean" value info))))

(defun ox-json-encode-string (value &optional info)
  "Encode VALUE to JSON as string or null.

INFO is the plist of export options.

Also accepts symbols."
  (when (and value (symbolp value))
    (setq value (symbol-name value)))
  (cond
    ((not value)
      "null")
    ((stringp value)
      ; Strip properties, doesn't make a difference to the final output to file
      ; but makes viewing intermediate output while debugging much less messy
      (json-encode-string (substring-no-properties value)))
    (t
      (ox-json--type-error "string or symbol" value info))))

(defun ox-json-encode-number (value &optional info)
  "Encode VALUE to JSON as number or null.

INFO is the plist of export options."
  (cond
    ((numberp value)
      (json-encode-number value))
    ((not value)
      "null")
    (t
      (ox-json--type-error "number" value info))))

(cl-defun ox-json-encode-array-raw (array &optional _info &key single-line omit-empty)
  "Encode array to JSON given its already-encoded items.

ARRAY is a list of strings with encoded JSON data.
INFO is the plist of export options.
If :single-line is non-nil will put all items on same line, otherwise will use
one line per item.

Items equal to `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no items remain, return `ox-json-omit'."
  ; Filter out ox-json-omit values
  (setq array (cl-remove ox-json-omit array :test #'eq))
  (if array
    ; Non-empty
    (if single-line
      (format "[%s]" (s-join ", " array))
      (format "[\n%s\n]" (s-join ",\n" array)))
    ; Empty
    (if omit-empty ox-json-omit "[]")))

(cl-defun ox-json-encode-alist-raw (data-type alist &optional info &key omit-empty)
  "Encode alist ALIST containing pre-encoded values into JSON object.

DATA-TYPE is the data type string of the returned object.
INFO is the plist of export options.

Pairs whose value is `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no pairs remain, return `ox-json-omit'."
  ; Filter out ox-json-omit values
  (setq alist
    (cl-loop
      for (key . value) in alist
      unless (eq value ox-json-omit)
      collect (cons key value)))
  (if (and omit-empty (not alist))
    ox-json-omit
    (let ((data-type-property (plist-get info :json-data-type-property)))
      (when (and data-type-property data-type)
        (push (cons data-type-property (json-encode-string data-type)) alist))
      (format "{\n%s\n}"
        (s-join ",\n"
          (cl-loop
            for (key . value) in alist
            collect (format "%s: %s" (json-encode-key key) (s-trim value))))))))

(cl-defun ox-json-encode-plist-raw (data-type plist &optional info &key omit-empty)
  "Encode plist PLIST containing pre-encoded values into JSON object.

DATA-TYPE is the data type string of the returned object.
INFO is the plist of export options.

Pairs whose value is `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no pairs remain, return `ox-json-omit'."
  (ox-json-encode-alist-raw data-type (ox-json--plist-to-alist plist) info
    :omit-empty omit-empty))

(defun ox-json-encode-auto (value &optional info)
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
      (if (ox-json--is-node value)
        (ox-json-export-data value info)
        (ox-json-encode-array value info)))
    (t
      (ox-json--error info "Can't automatically encode value of type %S" (type-of value)))))

(defun ox-json--get-type-encoder (typekey &optional info)
  "Get the type encoder function for type symbol TYPEKEY.

INFO is the plist of export options."
  (ox-json--plists-get typekey
    (plist-get info :json-exporters)
    ox-json-default-type-exporters))

(defun ox-json-encode-with-type (type value info)
  "Encode a VALUE to JSON given its type.

TYPE is a key in the plist under the :json-exporters option. It may also be a list
containing the key followed by additional arguments to pass to the encoder
function.
INFO is the plist of export options."
  (let* ((typekey (if (listp type) (car type) type))
         (args (if (listp type) (cdr type) nil))
         (encoder (ox-json--get-type-encoder typekey info)))
    (if encoder
      (apply encoder value info args)
      (ox-json--error info "Unknown type symbol %s" type))))

(cl-defun ox-json-encode-array (array &optional info (itemtype t) &key single-line omit-empty)
  "Encode the list ARRAY as a JSON array.

INFO is the plist of export options.
ITEMTYPE is optional and is the type to pass to `ox-json-encode-with-type'
to encode the items of the array. By default `ox-json-encode-auto' is used.
If :single-line is non-nil will put all items on same line, otherwise will use
one line per item.

Items equal to `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no items remain, return `ox-json-omit'."
  (ox-json-encode-array-raw
    (cl-loop
      for item in array
      unless (eq item ox-json-omit)
      collect (ox-json-encode-with-type itemtype item info))
    info
    :single-line single-line
    :omit-empty omit-empty))

(cl-defun ox-json-encode-alist (data-type alist &optional info &key (valuetype t) omit-empty)
  "Encode the alist ALIST as a JSON object.

DATA-TYPE is a data type string to add to the JSON object.
INFO is the plist of export options.
:valuetype is the type to pass to `ox-json-encode-with-type' to encode the
values of each key-value pair. By default `ox-json-encode-auto' is used.

Pairs whose value is `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no pairs remain, return `ox-json-omit'."
  (ox-json-encode-alist-raw
    data-type
    (cl-loop
      for (key . value) in alist
      unless (eq value ox-json-omit)
      collect (cons key (ox-json-encode-with-type valuetype value info)))
    info
    :omit-empty omit-empty))

(cl-defun ox-json-encode-plist (data-type plist &optional info &key (valuetype t) omit-empty)
  "Encode the plist PLIST as a JSON object.

DATA-TYPE is a data type string to add to the JSON object.
INFO is the plist of export options.
:valuetype is the type to pass to `ox-json-encode-with-type' to encode the
values of each key-value pair. By default `ox-json-encode-auto' is used.

Pairs whose value is `ox-json-omit' are omitted from the output.
If :omit-empty is non-nil and no pairs remain, return `ox-json-omit'."
  (ox-json-encode-alist
    data-type
    (ox-json--plist-to-alist plist)
    info
    :valuetype valuetype
    :omit-empty omit-empty))

(defun ox-json-make-alist (info properties)
  "Make an alist with JSON-encoded values of heterogeneous types.

INFO is the plist of export options.
PROPERTIES is a list of (key type value) forms for each property of the JSON
object. Each value will be JSON-encoded with `ox-json-encode-with-type'
according to the type symbol given. Values with a type of nil will be considered
to be already encoded. Entries whose value is `ox-json-omit' are omitted."
  (cl-loop
    for (key type value) in properties
    unless (eq value ox-json-omit)
    collect
    (cons
      key
      (if type
        (ox-json-encode-with-type type value info)
        value))))

(defun ox-json-make-object (data-type info properties)
  "Make an encoded JSON object from heterogeneous data.

DATA-TYPE is the data type string to add to the object.
INFO is the plist of export options.
PROPERTIES is interpreted as in `ox-json-make-alist'."
  (ox-json-encode-alist-raw data-type (ox-json-make-alist info properties) info))


(provide 'ox-json-encode)

;;; ox-json-encode.el ends here
