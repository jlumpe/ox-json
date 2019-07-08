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
    org              ,#'org-json-export-data
    secondary-string ,#'org-json-export-data
    array            ,#'org-json-encode-array
    plist            ,#'org-json-encode-plist
    alist            ,#'org-json-encode-alist
    t                ,#'org-json-encode-auto))

;;; Variables
(defgroup org-json nil "Customization for the ox-json package" :group 'outline)


;;; Utility code

(defmacro org-json--debug-print (expr)
  (let ((template (format "%s = %%S\n" expr)))
  `(princ (format ,template ,expr))))


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


(defun org-json--debug-print-plist (plist)
  (cl-loop for i from 0 to (- (length plist) 1) by 2
    do (let ((key (nth i plist))
              (value (nth (+ i 1) plist)))
         (princ (format "(%S . %S)\n" key value)))))


(cl-defmacro org-json--loop-plist ((key value plist) &body body)
  "Bind KEY and VALUE to each key-value pair in PLIST and execute BODY, returning results in list."
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
  "Check if Value is an org element/object."
  (and
    (listp value)
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
  `(
     (:json-data-type-property nil "json-data-type-property" "$$data_type")
     (:json-exporters nil nil nil)
     (:json-property-types nil nil nil)
     (:json-strict nil nil nil)
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

(defun org-json--type-error (type value info)
  "Encode or signal an error when asked to encode a value that is not of the expected type."
  (org-json--error info "Expected %s, got %S" type value))


;;; Encoders for generic data types

(defun org-json-encode-bool (value &optional info)
  "Encode VALUE to JSON as boolean."
  (cond
    ((equal value t)
      "true")
    (value
      (org-json--type-error "boolean" value info))
    (t
      "false")))

(defun org-json-encode-string (value &optional info)
  "Encode VALUE to JSON as string or null.

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
  "Encode VALUE to JSON as number or null."
  (cond
    ((numberp value)
      (json-encode-number value))
    ((not value)
      "null")
    (t
      (org-json--type-error "number" value info))))

(defun org-json-encode-array-raw (items &optional info)
  "Encode array to JSON given its already-encoded items."
  (if items
    (format "[\n%s\n]" (s-join ",\n" items))
    "[]"))

(defun org-json-encode-alist-raw (data-type alist &optional info)
  "Encode alist ALIST with encoded values into JSON object with data-type DATA-TYPE.

The values are expected to be JSON-encoded already, keys are not."
  (format "{\n%s\n}"
    (s-join
      ",\n"
      (cl-loop
        with initial = (cons (plist-get info :json-data-type-property) (json-encode-string data-type))
        for (key . value) in (cons initial alist)
        collect (format "%s: %s" (json-encode-key key) (s-trim value))))))

(defun org-json-encode-plist-raw (data-type plist &optional info)
  "Encode plist PLIST with encoded values into JSON object with data-type DATA-TYPE.

The values are expected to be JSON-encoded already, keys are not."
  (org-json-encode-alist-raw data-type (org-json--plist-to-alist plist) info))

(defun org-json-encode-auto (value &optional info)
  "Encode a value to JSON when its type is not known ahead of time.

Handles strings, numbers, and org elements/objects without a problem.
Non-empty lists which are not elements/objects are recursively encoded as
JSON arrays. Symbols are encoded as strings except for t which is encoded
as true.  This function cannot tell whether a nil value should correspond to an
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
  (let* ((typekey (if (listp type) (car type) type))
         (args (if (listp type) (cdr type) nil))
         (encoder (org-json--get-type-encoder typekey info)))
    (if encoder
      (apply encoder value info args)
      (org-json--error info "Unknown type symbol %s" type))))

(cl-defun org-json-encode-array (array &optional info (itemtype t))
  (let ((encoder (org-json--get-type-encoder itemtype info)))
    (if encoder
      (org-json-encode-array-raw
        (cl-loop
          for item in array
          collect (funcall encoder item info))
        info)
      (org-json--error info "Unknown type symbol %s" itemtype))))

(cl-defun org-json-encode-alist (data-type alist &optional info (valuetype t))
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
  (org-json-encode-alist
    data-type
    (org-json--plist-to-alist plist)
    info
    valuetype))


;;; Transcoders for org data

(defun org-json--end-array-item (node string info)
  "Add newline to encoded node, plus comma if not last child."
  (concat string (if (org-export-last-sibling-p node info) "\n" ",\n")))


(defun org-json-export-data (data info)
  "Like `org-export-data' but properly format secondary strings as arrays."
  (let ((exported (s-trim (org-export-data data info))))
    (if (and (listp data) (not (org-json--is-node data)))
      (format (if (> (length data) 1) "[\n%s\n]" "[%s]") exported)
      exported)))


(defun org-json--encode-contents (contents)
  "Convert concatenated, encoded contents into proper JSON list by surrounding with brackets."
  (if contents
    (format "[\n%s\n]" (s-trim contents))
    "[]"))


(defun org-json--get-doc-info-alist (info)
  "Get alist of top-level document properties (values already encoded)."
  `(
     (title . ,(org-json-export-data (plist-get info :title) info))
     (file_tags . ,(json-encode-list (plist-get info :filetags)))
     (author . ,(org-json-export-data (plist-get info :author) info))
     (creator . ,(org-json-encode-string (plist-get info :creator)))
     (date . ,(org-json-encode-string (plist-get info :date)))
     (description . ,(org-json-encode-string (plist-get info :description)))
     (email . ,(org-json-encode-string (plist-get info :email)))
     (language . ,(org-json-encode-string (plist-get info :language)))
     ))


(defun org-json-transcode-template (contents info)
  (let* ((docinfo (org-json--get-doc-info-alist info))
         (alist
          `(
             ,@docinfo
             (contents . ,(org-json--encode-contents contents)))))
    (org-json-encode-alist-raw "org-document" alist info)))


(defun org-json-transcode-plain-text (text info)
  (org-json--end-array-item text (json-encode-string text) info))


(defun org-json-export-property-value (node-type key value info)
  (org-json-encode-auto value info))


(defun org-json--export-properties (node info)
  (org-json-encode-alist-raw
    "mapping"
    (org-json--loop-plist (key value (org-json-node-properties node))
      unless (eq key :parent)
        collect (cons key (org-json-export-property-value node key value info)))
    info))


(cl-defun org-json-transcode-base (node contents info &key properties extra)
  "Base transcoding function for all element/object types."
  (unless properties
    (setq properties (org-json--export-properties node info)))
  (let* ((node-type (org-element-type node))
         (object-alist
           `(
             (type . ,(json-encode-string (symbol-name node-type)))
             ,@extra
             (properties . ,properties)
             (contents . ,(org-json--encode-contents contents))))
         (strval (org-json-encode-alist-raw "org-node" object-alist info)))
    (org-json--end-array-item node strval info)))


(defun org-json-transcode-headline (node contents info)
  (org-json-transcode-base node contents info
    :extra `(
       (ref . ,(json-encode-string (org-export-get-reference node info))))))


(provide 'ox-json)

;;; ox-json.el ends here
