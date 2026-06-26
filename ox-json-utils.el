;;; ox-json-utils.el --- Utility functions for ox-json  -*- lexical-binding: t; -*-

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
      (org-element-properties-map
       (lambda (name value)
         (setq expanded-properties (plist-put expanded-properties name value)))
       node t)
      expanded-properties)
    ; for org versions < 9.7, just return the property list, which is the second
    ; element of the list
    (cadr node)))

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
