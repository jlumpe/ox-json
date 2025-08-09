;;; Helper code for tests

(require 'ox-json)
(require 'cl-lib)
(require 's)
(require 'subr-x)


;;; Get backend and default options
(setq
  backend (org-export-get-backend 'json)
  info (org-export-get-environment backend nil '(:json-strict t))
  info-nonstrict (org-combine-plists info '(:json-strict nil)))


;; General helper functions

(defun remove-ws (s)
  (s-replace-regexp "[[:blank:]\n]+" "" s))


(defun alist-get-equal (key alist)
  "(alist-get) with testfn = equal, making it suitable for string keys."
  (alist-get key alist nil nil #'equal))


;; Basic JSON helper functions

(defun encoded= (a b)
  "Check whether encoded JSON values are identical, up to differences in whitespace."
  (string= (remove-ws a) (remove-ws b)))

(put
  'encoded=
  'ert-explainer
  (lambda (a b)
      (format "Encoded values differ: %S %S" (remove-ws a) (remove-ws b))))


(defun normalize-key (key)
  "Get JSON key value from string or symbol, removing colon at beginning of symbol names."
  (cond
    ((stringp key)
      key)
    ((symbolp key)
      (s-replace-regexp "^:" "" (symbol-name key)))
    (t
      (error "Keys must be strings or symbols"))))


(defun json-obj (info type &rest properties)
  "Create decoded JSON object to compare against."
  (let ((obj (make-hash-table :test 'equal))
         (data-type-property (plist-get info :json-data-type-property)))
    (if (and data-type-property type)
      (puthash data-type-property type obj))
    (ox-json--loop-plist (key value properties)
      do (puthash (normalize-key key) value obj))
    obj))


(defmacro with-json-decode-explicit (&rest body)
  "Set JSON decoding settings to make things less ambiguous.

Decodes arrays as vectors, objects as hash maps, null as :json-null,
and false as :json-false. Avoids all ambiguity around nil values
and plists/alists because nothing is ever decoded into any type of
list."
  `(let ((json-null :json-null)
         (json-false :json-false)
         (json-array-type 'vector)
         (json-object-type 'hash-table))
    ,@body))


;;; Recursive JSON comparison


(defun -check-ignore-list-empty (ignore)
  (if ignore
    (error
      (format
        "Internal error: ignore list %s passed for non-object at path %s"
        ignore
        (reverse path)
      ))))


(defun -json-compare (data1 data2 opts path &optional ignore)
  "Recursively compare two decoded JSON values."
  ; Compare types
  (should (-json-cmp-types data1 data2 opts path))
    ; Comparison based on type
    (cl-case (type-of data1)
      ((hash-table)
        (let (
            (cmp-obj-fun (alist-get 'cmp-obj opts))
          )
          (funcall cmp-obj-fun data1 data2 opts path ignore)
        )
      )
      ((vector)
        (-check-ignore-list-empty ignore)
        (-json-cmp-arrays data1 data2 opts path))
      ; Scalars, use direct equality
      (t
        (-check-ignore-list-empty ignore)
        (should (-json-cmp-scalars data1 data2 opts path)))))


(defun -json-cmp-failed (opts path msg &rest rem)
    "Make the error message for json-compare."
    (format
        "Comparison failed at %s: %s"
        (if path
            (format "path %S" (reverse path))
            "top level")
        (apply 'format msg rem)))


(defun -json-cmp-types (data1 data2 opts path)
  (let ((type1 (if data1 (type-of data1)))
        (type2 (if data2 (type-of data2))))
    (equal type1 type2)))

(put
    '-json-cmp-types
    'ert-explainer
    (lambda (data1 data2 opts path)
      (let ((type1 (if data1 (type-of data1)))
            (type2 (if data2 (type-of data2))))
        (-json-cmp-failed opts path "Values have different type (%s, %s)" type1 type2))))


(defun -json-cmp-scalars (value1 value2 opts path)
    (equal value1 value2))

(put
    '-json-cmp-scalars
    'ert-explainer
    (lambda (value1 value2 opts path)
        (-json-cmp-failed opts path "Scalar values differ (%S, %S)" value1 value2)))


(defun -json-cmp-has-key (name key keylist opts path)
    (member key keylist))

(put
    '-json-cmp-has-key
    'ert-explainer
    (lambda (name key keylist opts path)
        (-json-cmp-failed opts path "%s missing property \"%s\"" name key)))


(defun -json-cmp-lengths (len1 len2 opts path)
    (= len1 len2))

(put
    '-json-cmp-lengths
    'ert-explainer
    (lambda (len1 len2 opts path)
        (-json-cmp-failed opts path "Arrays have different lengths (%d, %d)" len1 len2)))


(defun -json-cmp-objects (table1 table2 opts path ignore)
  "Compare JSON objects decoded as hash tables."
  (let (
      (keys1 (hash-table-keys table1))
      (keys2 (hash-table-keys table2))
      (key-ignore)
    )
    ; All keys in first
    (dolist (key1 keys1)
      (setq key-ignore (alist-get-equal key1 ignore))
      (unless (equal key-ignore t)
        ; Key missing from table 2
        (should (-json-cmp-has-key "Object 2" key1 keys2 opts path))
        ; Recursively compare values
        (-json-compare
          (gethash key1 table1)
          (gethash key1 table2)
          opts
          (cons key1 path)
          key-ignore
        )
      )
    )
    ; Check keys in table 2 missing in 1
    (dolist (key2 keys2)
      (setq key-ignore (alist-get-equal key2 ignore))
      (unless (equal key-ignore t)
        (should (-json-cmp-has-key "Object 1" key2 keys1 opts path))
      )
    )
  )
)


(defun -json-cmp-arrays (vec1 vec2 opts path)
  "Compare JSON arrays decoded as vectors."
  (let ((len1 (length vec1))
        (len2 (length vec2)))
    ; Compare lengths
    (should (-json-cmp-lengths len1 len2 opts path))
    ; Compare values
    (cl-loop
      for i from 0 to (- len1 1)
      do (-json-compare (aref vec1 i) (aref vec2 i) opts (cons i path)))))


(cl-defun json-compare (data1 data2 &key ignore (cmp-obj #'-json-cmp-objects))
  "Recursively compare two decoded JSON values.

  IGNORE is an alist specifying keys to ignore if the two values are objects. If the value is t, the
  key is ignored entirely. If the value is another alist, it is used recursively to compare the
  objects under that key. It is invalid for IGNORE to be used with non-objects.

  CMP-OBJ is the function to be used to compare objects. It should have the same signature as
  (-json-cmp-objects) and probably call it internally.
  "
  (let (
      (opts `(
        (cmp-obj . ,cmp-obj)
      ))
    )
    (-json-compare data1 data2 opts nil ignore)
  )
)


(defun decode-compare (encoded data2)
  "Compare exported/encoded string against decoded value."
  (with-json-decode-explicit
    (json-compare (json-read-from-string encoded) data2)))


(provide 'ox-json-test-helpers)
