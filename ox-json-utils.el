;;; ox-json-utils.el --- Utility functions for ox-json  -*- lexical-binding: t; -*-

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

;; Generic and Org-mode utility functions for ox-json.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'cl-lib)
(require 's)
(require 'org-element)


;;; Generic utility code

(defun ox-json--merge-alists (&rest alists)
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

(defun ox-json--plists-get-default (key default &rest plists)
  "Try getting value for KEY from each plist in PLISTS in order, returning DEFAULT if not found."
  (cl-loop
    for plist in plists
    if (plist-member plist key)
      return (plist-get plist key)
    finally return default))

(defun ox-json--plists-get (key &rest plists)
  "Try getting value for KEY from each plist in PLISTS in order, returning nil if not found."
  (apply #'ox-json--plists-get-default key nil plists))

(cl-defmacro ox-json--loop-plist ((key value plist) &body body)
  "Bind KEY and VALUE to each key-value pair in PLIST and execute BODY within a `cl-loop'."
  (let ((plist-var (make-symbol "plist")))
    `(let ((,plist-var ,plist)
            (,key nil)
            (,value nil))
      (cl-loop
        while ,plist-var
        do (setq

              ,key (car ,plist-var)
              ,value (cadr ,plist-var)
              ,plist-var (cddr ,plist-var))
        ,@body))))

(defun ox-json--plist-to-alist (plist)
  "Convert plist PLIST to alist."
  (ox-json--loop-plist (key value plist)
    collect (cons key value)))

(defun ox-json--json-key-string (key)
  "Return the JSON object key string for KEY, mirroring `json-encode-key'."
  (cond
    ((symbolp key)
     (let ((name (symbol-name key)))
       (if (eq (aref name 0) ?\:)
           (substring name 1)
         name)))
    ((stringp key) key)
    (t (format "%s" key))))

(defun ox-json--sort-alist-by-key (alist)
  "Return a copy of ALIST sorted alphabetically by JSON object key."
  (sort (copy-sequence alist)
        (lambda (a b)
          (string< (ox-json--json-key-string (car a))
                   (ox-json--json-key-string (car b))))))


;;; Org-mode utility code

(defun ox-json-node-properties (node)
  "Org v9.7 introduced two significant changes to the AST that must be
considered when enumerating a node's properties:

     1. Some properties which were previously present in the property
     list (e.g. :begin and :end) are now stored as elements of a vector
     under the :standard-properties key in the property list.

     2. Property values can now be 'deferred', meaning they are not
     calculated until accessed via a getter function like
     ~org-element-property~.

~org-element-properties-map~ is now the recommended way to traverse a
node's properties and handles both of these changes."
  (if (fboundp 'org-element-properties-map)
    (let ((expanded-properties nil))
      ;; Org 9.7 stores some headline properties (e.g. :archivedp,
      ;; :footnote-section-p) as shared deferred values with
      ;; auto-undefer-p nil.  Undefer with `force' so resolved values
      ;; replace those placeholders; otherwise they encode as true.
      (org-element-properties-map
       (lambda (name value)
         (setq expanded-properties (plist-put expanded-properties name value)))
       node 'force)
      expanded-properties)
    ; for org versions < 9.7, just return the property list, which is the second
    ; element of the list
    (cadr node)))

(defun ox-json--node-structural-path (datum)
  "Return DATUM's path as a list of sibling indices from the parse-tree root.
Each index is the 0-based position of the node within its parent's
`org-element-contents'."
  (let (path node parent)
    (setq node datum)
    (while (setq parent (org-element-property :parent node))
      (push (or (cl-position node (org-element-contents parent) :test #'eq) 0)
            path)
      (setq node parent))
    path))

(defun ox-json--format-structural-ref (datum)
  "Return a deterministic ref string for DATUM based on its structural path.
The ref is stable across Org versions because it does not depend on buffer
positions, which can shift between parser versions."
  (let* ((type (org-element-type datum))
         (path (ox-json--node-structural-path datum))
         (key (format "%s:%s" type (mapconcat #'number-to-string path ","))))
    (format "org%s" (substring (secure-hash 'sha256 key) 0 7))))

(defun ox-json--is-node (value)
  "Check if VALUE is an org element/object."
  (and
    (listp value)
    (listp (cdr value))
    (>= (length value) 2)
    (symbolp (car value))
    (listp (cadr value))))

(defun ox-json-timestamp-isoformat (timestamp suffix _info &optional zone)
  "Convert timestamp time to ISO 8601 format.

TIMESTAMP is a timestamp object from an Org mode parse tree.
SUFFIX is either \"start\" or \"end\".
ZONE is a time zone to pass to `format-time-string'."
  (let* ((minute (org-element-property (intern (concat ":minute-" suffix)) timestamp))
         (hour (org-element-property (intern (concat ":hour-" suffix)) timestamp))
         (day (org-element-property (intern (concat ":day-" suffix)) timestamp))
         (month (org-element-property (intern (concat ":month-" suffix)) timestamp))
         (year (org-element-property (intern (concat ":year-" suffix)) timestamp)))
    (cond
      ; With time
      (hour
        (format-time-string "%Y-%m-%dT%H:%M:00" (encode-time 0 minute hour day month year zone) zone))
      ; Date only (otherwise nil)
      (year
        (format-time-string "%Y-%m-%d" (encode-time 0 0 0 day month year zone) zone)))))

(defun ox-json-headline-tags-all (headline info)
  "Return all tags for HEADLINE, including inherited tags and filetags.

HEADLINE is a headline element from the parse tree.
INFO is the plist of export options.

`org-get-tags' depends on point being at the headline in the Org buffer.
Org 9.6+ sets point during export transcoding; older versions do not."
  (let ((begin (org-element-property :begin headline)))
    (if begin
        (with-current-buffer
            (or (plist-get info :buffer)
                (org-element-property :buffer headline)
                (current-buffer))
          (save-excursion
            (goto-char begin)
            (org-get-tags)))
      (org-get-tags))))

(defun ox-json--is-drawer-property-name (name &optional _info)
  "Try to determine if a headline property name came from a property drawer.

NAME is the property name as symbol or string."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (s-uppercase-p name))

(defun ox-json--separate-drawer-properties (properties info)
  "Separate drawer properties from a headline's property plist.

PROPERTIES is a plist of the headline's properties, as from
`ox-json-node-properties'.

INFO is the plist of export options.

Returns a cons cell containing two plists, the regular properties in the car
and the drawer properties in the cdr."
  (let ((regular-props nil)
        (drawer-props nil))
    (ox-json--loop-plist (name value properties)
      do (if (ox-json--is-drawer-property-name name info)
           ; Property drawer
           (let* ((realname (intern (s-replace "+" "" (symbol-name name))))
                  (existing (plist-get drawer-props realname))
                  (strval (format "%s" value))
                  (newval (if existing
                             (concat existing " " strval)
                             strval)))
             (setq drawer-props (plist-put drawer-props realname newval)))
           ; Regular property
           (setq regular-props (plist-put regular-props name value))))
    (cons regular-props drawer-props)))


(provide 'ox-json-utils)

;;; ox-json-utils.el ends here
