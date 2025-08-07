; Test full export

(require 'ox-json)
(require 'ox-json-test-helpers)


; JSON object keys to ignore in comparison based on $$data_type
; Alist, values are ignore alists to pass to (json-compare-exported)
(setq ignore-by-data-type
  '(
    ("org-document"
      ("properties"
        ("creator" . t)
      )
    )
    ("org-node"
      ("ref" . t)
    )
  )
)



(defun -check-ignore-list-empty (ignore)
  (if ignore
    (error
      (format
        "Internal error: ignore list %s passed for non-object at path %s"
        ignore
        (reverse path)
      )
    )
  )
)


(defun json-compare-exported (data1 data2 &optional path ignore)
  "Compare two exported JSON values.

  IGNORE is an alist specifying keys to ignore on objects. If the value is t, the key is ignored
  entirely. If the value is another alist, it is used recursively to compare the objects under that
  key.

  It is invalid for IGNORE to be used with non-objects.
  "
  ; Compare types
  (should (-json-cmp-types data1 data2 path))
  ; Comparison based on type
  (cl-case (type-of data1)
    ((hash-table)
      (-json-cmp-exported-objects data1 data2 path ignore)
    )
    ((vector)
      (-check-ignore-list-empty ignore)
      (-json-cmp-exported-arrays data1 data2 path)
    )
    ; Scalars, use direct equality
    (t
      (-check-ignore-list-empty ignore)
      (should (-json-cmp-scalars data1 data2 path))
    )
  )
)


(defun -json-cmp-exported-objects (table1 table2 path &optional ignore)
  "Basic recursive object comparison, optionally ignoring specific keys."
  (let* (
      (keys1 (hash-table-keys table1))
      (keys2 (hash-table-keys table2))
      (data-type (gethash "$$data_type" table1))
      (data-type-ignore (alist-get-equal data-type ignore-by-data-type))
      (ignore-extended (append ignore data-type-ignore))
      (key-ignore)
    )
    ; Compare data type first
    (json-compare-exported
      data-type
      (gethash "$$data_type" table2)
      (cons "$$data_type" path)
    )
    ; All keys in first
    (dolist (key1 keys1)
      (setq key-ignore (alist-get-equal key1 ignore-extended))
      (unless (equal key-ignore t)
        ; Key missing from table 2
        (should (-json-cmp-has-key "Object 2" key1 keys2 path))
        ; Recursively compare values
        (json-compare-exported
          (gethash key1 table1)
          (gethash key1 table2)
          (cons key1 path)
          key-ignore
        )
      )
    )
    ; Check keys in table 2 missing in 1
    (dolist (key2 keys2)
      (setq key-ignore (alist-get-equal key2 ignore-extended))
      (unless (equal key-ignore t)
        (should (-json-cmp-has-key "Object 1" key2 keys1 path))
      )
    )
  )
)


(defun -json-cmp-exported-arrays (vec1 vec2 path)
  (let
    (
      (len1 (length vec1))
      (len2 (length vec2))
    )
    ; Compare lengths
    (should (-json-cmp-lengths len1 len2 path))
    ; Compare values
    (cl-loop
      for i from 0 to (- len1 1)
      do (json-compare-exported (aref vec1 i) (aref vec2 i) (cons i path))
    )
  )
)


(ert-deftest test-export ()
  "Test export of full org document."
  (json-decode-explicit
    (let*
      (
        (exported-string
          (with-current-buffer (find-file-noselect "test.org")
            (with-current-buffer (ox-json-export-to-buffer)
              (buffer-string))))
        (exported-data (json-read-from-string exported-string))
        (test-data (json-read-file "test.json"))
      )
      ; Check $$data_type key is present and has correct value
      (should (string= (gethash "$$data_type" exported-data) "org-document"))
      ; Compare value to test.json
      (json-compare-exported exported-data test-data)
    )
  )
)
