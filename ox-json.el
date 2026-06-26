;;; ox-json.el --- JSON export backend for Org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 - 2023 Jared Lumpe

;; Author: Jared Lumpe <jared@jaredlumpe.com>
;; Version: 0.3.0
;; Keywords: outlines
;; Homepage: https://github.com/jlumpe/ox-json

;; Package-Requires: ((emacs "26.1") (org "9") (s "1.12"))

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

;; Org mode export backend for exporting the document syntax tree to JSON.
;; The main entry points are `ox-json-export-as-json' and
;; `ox-json-export-to-json'. It can also be used through the built-in
;; export dispatcher through `org-export-dispatch'.

;; Export options:

;; :json-data-type-property (string) - This the name of a property added to all
;;   JSON objects in export to differentiate between structured data and
;;   ordinary key-value mappings. Its default value is "$$data_type". Setting
;;   to nil prevents the property being added altogether.

;; :json-exporters - plist containing exporter functions for different data
;;   types. The keys appear in :json-property-types and can also be used with
;;   `ox-json-encode-with-type'. Functions are called with the value to be
;;   exported and the export info plist. Default values stored in
;;   `ox-json-default-type-exporters'.

;; :json-property-types (plist) - Sets the types of properties of specific
;;   elements/objects. Nested set of plists - the top level is keyed by element
;;   type (see `org-element-type') and the second level by property name (used
;;   with `org-element-property'). Values in 2nd level are keys in the
;;   :json-exporters plist and are used to pick the function that will export
;;   the property value. Properties with a type of t will be encoded using
;;   `ox-json-encode-auto', but this sometimes can produce undesirable
;;   results. The "all" key contains the default property types for all element
;;   types. This option overrides the defaults set in
;;   `ox-json-default-property-types'.

;; :json-strict (bool) - If true an error will be signaled when problems are encountered
;;   in exporting a data structure. If nil the data structure will be exported as an
;;   object containing an error message. Defaults to nil.

;; :json-include-extra-properties (bool) - Whether to export node properties not listed
;;   in the :json-property-types option. If true these properties will be exported
;;   using `ox-json-encode-auto'.

;; :json-postprocess (symbol) - How to postprocess the final output. Values are `pretty'
;;   (indent properly), `minimal' (remove whitespace), and nil (nothing, maybe faster?).

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
(require 'ox-json-export)

;; Require json-mode if available, and tell the compiler the json-mode
;; function should be defined
(require 'json-mode nil t)
(declare-function json-mode "ext:json-mode.el")


;;; Define the backend

(org-export-define-backend 'json
  ;; Transcoders
  (ox-json--merge-alists
    '(
       (template . ox-json-transcode-template)
       (plain-text . ox-json-transcode-plain-text)
       (headline . ox-json-transcode-headline)
       (link . ox-json-transcode-link)
       (timestamp . ox-json-transcode-timestamp))
    ; Default for all remaining element/object types
    (cl-loop
      for type in (append org-element-all-elements org-element-all-objects)
      collect (cons type #'ox-json-transcode-base)))
  ;; Filters
  :filters-alist `(
    (:filter-final-output . ,#'ox-json-filter-final-output)
  )
  ;; Options
  :options-alist
  '(
     ; Property name specifying data type of exported JSON object
     (:json-data-type-property nil "json-data-type-property" "$$data_type")
     ; Overrides to ox-json-default-type-exporters
     (:json-exporters nil nil nil)
     ; Overrides to ox-json-default-property-types
     (:json-property-types nil nil nil)
     ; If non-nil abort on encountering errors, otherwise encode errors in output JSON
     (:json-strict nil nil nil)
     ; Include properties not defined in ox-json-default-property-types
     (:json-include-extra-properties nil nil t)
     ; How to post-process the final output
     (:json-postprocess nil nil 'pretty))
  ;; Menu
  :menu-entry
  '(?j "Export to JSON" (
	  (?J "As JSON buffer" ox-json-export-to-buffer)
	  (?j "To JSON file" ox-json-export-to-file))))


;;; User export functions

(defun ox-json-export-to-buffer
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a JSON buffer.

ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are the arguments to
`org-export-to-buffer'."
  ; Modified from org-html-export-as-html:
  (interactive)
  (let ((buffer (org-export-to-buffer 'json "*Org JSON Export*"
                  async subtreep visible-only body-only ext-plist)))
    ; Switch to json mode if available
    (when (fboundp 'json-mode)
      (with-current-buffer buffer
        (json-mode)))
    buffer))


(defun ox-json-export-to-file
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a JSON file.

ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, and EXT-PLIST are the arguments to
`org-export-to-file'."
  ; Modified from org-html-export-to-html:
  (interactive)
  (let ((file (org-export-output-file-name ".json" subtreep)))
    (org-export-to-file 'json file
      async subtreep visible-only body-only ext-plist)))


;;; Misc functions for exporting

(defun ox-json--init-backend (&optional ext-plist)
  "Initialize the export-options plist independent of any buffer.

Optional argument EXT-PLIST is a plist of options that override the default
values for the JSON back end.

Creates the \"communication channel\" plist that is passed as the \"info\"
argument to most export functions. Normally this is created inside functions
like `org-export-as', but it incorporates information from the current
Org mode buffer and so won't work when exporting agenda items from multiple
files.

Code copied from `org-export-as'."
  ;; (save-excursion
  ;;   (save-restriction
  (let* ((backend-name 'json)
         (org-export-current-backend 'json)
         (backend (org-export-get-backend 'json))
         (info (org-export--get-export-attributes backend)))
    ;; Update communication channel with environment.
    (setq info
      (org-combine-plists
        info (org-export-get-environment backend nil ext-plist)))
    ;; Install user's and developer's filters.
    (setq info (org-export-install-filters info))
    ;; Call options filters and update export options.  We do not
    ;; use `org-export-filter-apply-functions' here since the
    ;; arity of such filters is different.
    (dolist (filter (plist-get info :filter-options))
      (let ((result (funcall filter info backend-name)))
        (when result (setq info result))))
    info))


(provide 'ox-json)

;;; ox-json.el ends here
