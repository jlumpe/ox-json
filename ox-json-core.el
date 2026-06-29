;;; ox-json-core.el --- Core constants and variables for ox-json  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 - 2026 Jared Lumpe

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

;; Constants, customization group, and shared variables for ox-json.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'cl-lib)

(declare-function ox-json-encode-bool "ox-json-encode")
(declare-function ox-json-encode-string "ox-json-encode")
(declare-function ox-json-encode-number "ox-json-encode")
(declare-function ox-json-export-property-node "ox-json-export")
(declare-function ox-json-export-secondary-string "ox-json-export")
(declare-function ox-json-encode-array "ox-json-encode")
(declare-function ox-json-encode-plist "ox-json-encode")
(declare-function ox-json-encode-alist "ox-json-encode")
(declare-function ox-json-export-timestamp-property "ox-json-export")
(declare-function ox-json-encode-tag-string "ox-json-export")
(declare-function ox-json-encode-auto "ox-json-encode")


;;; Private constants

(defconst ox-json--absent (make-symbol "ox-json--absent")
  "Sentinel for `ox-json--plists-get-default' when KEY is not found.")

(defconst ox-json-default-type-exporters
  `(
    bool             ,#'ox-json-encode-bool
    string           ,#'ox-json-encode-string
    number           ,#'ox-json-encode-number
    node             ,#'ox-json-export-property-node
    secondary-string ,#'ox-json-export-secondary-string
    array            ,#'ox-json-encode-array
    plist            ,#'ox-json-encode-plist
    alist            ,#'ox-json-encode-alist
    timestamp        ,#'ox-json-export-timestamp-property
    tag-string       ,#'ox-json-encode-tag-string
    char             ,#'ox-json-encode-char
    t                ,#'ox-json-encode-auto)
  "Default exporter function for each element property type.

Plist mapping property symbols in
`ox-json-default-property-types' to exporter functions.  These can
be overridden with the :json-exporters option.")

(defconst ox-json-default-property-types
  '(
    all (
      ; Never include parent, leads to infinite recursion
      :parent nil
      :buffer nil
      ; These properties have to do with absolute buffer positions and thus probably aren't useful to export
      :begin nil
      :end nil
      :contents-begin nil
      :contents-end nil
      :robust-begin nil
      :robust-end nil
      :post-affiliated nil
      ; Parser-internal metadata: records the recursion depth used when the node was parsed (e.g. headline,
      ; greater-element, element, object). Not a property of document content - it reflects the org-element
      ; parser invocation and is almost always nil for a full parse. Introduced in org 9.6.
      :granularity nil
      ; These can be useful when converting from JSON to another format
      :pre-blank number
      :post-blank number)
    babel (
      :call string
      :inside-header string
      :arguments string
      :end-header string
      :value string
      :result (array string))
    clock (
      :duration string
      :status string
      :value timestamp)
    code (
      :value string)
    comment (
      :value string)
    comment-block (
      :value string)
    drawer (
      :drawer-name string)
    dynamic-block (
      :arguments string
      :block-name string
      :drawer-name string)
    entity (
      :ascii string
      :html string
      :latex string
      :latex-math-p bool
      :latin1 string
      :name string
      :use-brackets-p bool
      :utf-8 string)
    example-block (
      :label-fmt string
      :language string
      :number-lines string
      :options string
      :parameters string
      :preserve-indent bool
      :retain-labels bool
      :switches string
      :use-labels bool
      :value string)
    export-block (
      :type string
      :value string)
    export-snippet (
      :back-end string
      :value string)
    footnote-reference (
      :label string
      :type string)
    headline (
      :archivedp bool
      :closed timestamp
      :commentedp bool
      :deadline timestamp
      :footnote-section-p bool
      :level number
      :priority char
      :quotedp bool
      :raw-value string
      :scheduled timestamp
      :tags (array string)
      :title secondary-string
      :todo-keyword string
      :todo-type string)
    inline-babel-call (
      :call string
      :inside-header string
      :arguments string
      :end-header string
      :value string)
    inline-src-block (
      :language string
      :parameters string
      :value string)
    inlinetask (
      :closed timestamp
      :deadline timestamp
      :scheduled timestamp
      :title secondary-string)
    item (
      :bullet string
      :checkbox string
      :counter number
      :raw-tag string
      :tag secondary-string
      :structure nil)  ; TODO
    keyword (
      :key string
      :value string)
    latex-environment (
      :value string)
    latex-fragment (
      :value string)
    link (
      :application string
      :format string
      :path string
      :raw-link string
      :search-option string
      :type string)
    macro (
      :args (array string))
    node-property (
      :key string
      :value string)
    plain-list (
      ;; :structure array)
      :structure nil)
    planning (
      :closed timestamp
      :deadline timestamp
      :scheduled timestamp)
    radio-target (
      :raw-value string)
    special-block (
      :type string
      :raw-value string)
    src-block (
      :label-fmt string
      :language string
      :number-lines string
      :parameters string
      :preserve-indent bool
      :retain-labels bool
      :switches string
      :use-labels bool
      :value string)
    statistics-cookie (
      :value string)
    subscript (
      ; Uses integer value for true, disable strict bool encoding
      :use-brackets-p (bool nil))
    superscript (
      ; Same as subscript
      :use-brackets-p (bool nil))
    table (
      :tblfm t
      :type string
      :value string)
    table-row (
      :type string)
    target (
      :value string)
    timestamp (
      :day-end nil  ; number
      :day-start nil  ; number
      :hour-end nil  ; number
      :hour-start nil  ; number
      :minute-end nil  ; number
      :minute-start nil  ; number
      :month-end nil  ; number
      :month-start nil  ; number
      :raw-value string
      :repeater-type string
      :repeater-unit string
      :repeater-value number
      :type string
      :warning-type string
      :warning-unit string
      :warning-value number
      :year-end nil  ; number
      :year-start nil)  ; number
    verbatim (
      :value string))
  "Default type symbols for properties of all Org element/object types.

Nested set of plists.  Keys are element/object type symbols as
returned by `org-element-type', along with \"all\" which sets the
defaults for all types.  The values are plists mapping property
symbols (starting with colons) to type symbols in
`ox-json-default-type-exporters'.  A value of nil means to ignore
the property.

These can be overridden with the :json-property-types option.")

(defconst ox-json-default-property-values
  '(
    all (
      :pre-blank  0
      :post-blank 0
      :mode nil
      :secondary nil
      :cached nil
      :true-level nil
      :deferred nil
      :structure nil
    )
    headline (
      :tags nil
      :archivedp nil
      :commentedp nil
      :footnote-section-p nil
      :priority nil
      :todo-keyword nil
      :todo-type nil
      ;; :quotedp            nil
      ;; :closed             nil
      ;; :deadline           nil
      ;; :scheduled          nil)
    )
    item (
      :checkbox nil
      :counter nil
      :tag nil
    )
    link (
      :application nil
      :search-option nil
    )
    timestamp (
      :range-type nil
    )
  )
  "Default property values to omit from export.

Same nested-plist structure as `ox-json-default-property-types'.
Top-level keys are element/object type symbols or \"all\".
Inner plists map property symbols to the value for which the
property will NOT be exported.")


;;; Variables
(defgroup ox-json nil "Customization for the ox-json package." :group 'outline)


;;; Private globals

(defvar ox-json--current-node nil
  "The org node currently being transcoded, or the current export context.

Used for error reporting.")


(provide 'ox-json-core)

;;; ox-json-core.el ends here
