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

(require 'cl-lib)
(require 'json)
(require 'ox)
(require 'org-element)


;;; Variables
(defgroup org-json nil "Customization for the ox-json package" :group 'outline)

(defcustom org-json-data-type-property "$$data_type"
  "Property which indicates the data type of JSON objects."
  :type '(string))


;;; Utility code

(defmacro org-json--debug-print (expr)
  (let ((template (format "%s = %%S\n" expr)))
  `(princ (format ,template ,expr))))

(defun org-json--merge-alists (&rest alists)
  (cl-loop
    with keys = (make-hash-table :test 'equal)
    for alist in alists
    append
      (cl-loop
        for item in alist
        unless (gethash (car item) keys)
          collect item
        do (puthash (car item) t keys))))

(defun org-json--indent-string (string indent)
  "Insert INDENTATION at the beginning of each non-empty line in STRING."
  (replace-regexp-in-string "^" indent string))



;;; Define the backend

(org-export-define-backend 'json
  ;; Transcoders
  (org-json--merge-alists
    '(
      (template . org-json-transcode-template)
      (plain-text . org-json-transcode-plain-text)
      (headline . org-json-transcode-headline)
      (headline . org-json-transcode-generic))
    (cl-loop
      for type in org-element-all-elements
      collect (cons type #'org-json-transcode-generic))
    (cl-loop
      for type in org-element-all-objects
      collect (cons type #'org-json-transcode-generic)))
  ;; Filters
  :filters-alist '()
  ;; Options
  :options-alist '()
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


;;; Transcoders

(defun org-json--end-array-item (node info string)
  "Add newline to encoded node, plus comma if not last child."
  (concat string (if (org-export-last-sibling-p node info) "\n" ",\n")))

(defun org-json--transcode-object-alist-raw (data-type properties)
  "Transcode alist PROPERTIES into JSON object with data-type DATA-TYPE.

The values are expected to be JSON-encoded already, keys are not."
  (format "{\n%s\n}"
    (s-join
      ",\n"
      (cl-loop
        with initial = (cons org-json-data-type-property (json-encode-string data-type))
        for (key . value) in (cons initial properties)
        collect (format "  %s: %s" (json-encode-key key) value)))))

(defun org-json--transcode-contents (contents)
  "Convert concatenated, encoded contents into proper JSON list."
  (if contents
    (format "[\n%s\n  ]" (org-json--indent-string (s-trim-right contents) "    "))
    "[]"))

(defun org-json-transcode-template (contents info)
  (org-json-transcode-object-alist
    "org-document"
    `(
      (contents . ,(org-json--transcode-contents contents)))))

(defun org-json-transcode-plain-text (text info)
  (org-json--end-array-item text info (json-encode-string text)))

(defun org-json-transcode-generic (node contents info)
  (let ((node-type (org-element-type node)))
    (org-json--end-array-item node info
      (org-json--transcode-object-alist-raw
        "org-node"
        `(
          (type . ,(json-encode-string (symbol-name node-type)))
          (contents . ,(org-json--transcode-contents contents)))))))

(defun org-json-transcode-headline (node contents info)
  (org-json-transcode-generic node contents info))



(provide 'ox-json)

;;; ox-json.el ends here
