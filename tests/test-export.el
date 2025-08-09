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


(defun -json-cmp-exported-objects (table1 table2 opts path ignore)
  "Override to JSON object comparison, ignoring keys based on data type."
  (let* (
      (data-type (gethash "$$data_type" table1))
      (data-type-ignore (alist-get-equal data-type ignore-by-data-type))
      (ignore-extended (append ignore data-type-ignore))
    )
    ; Make sure data types are equal
    (-json-cmp-scalars
      data-type
      (gethash "$$data_type" table2)
      opts
      (cons "$$data_type" path)
    )
    ; Standard comparison with extended ignore list
    (-json-cmp-objects table1 table2 opts path ignore-extended)
  )
)


(ert-deftest test-export ()
  "Test export of full org document."
  (with-json-decode-explicit
    (let*
      (
        (ext-plist '(:json-strict t))
        (exported-string
          (with-current-buffer (find-file-noselect "test.org")
            (with-current-buffer (ox-json-export-to-buffer nil nil nil nil ext-plist)
              (buffer-string))))
        (exported-data (json-read-from-string exported-string))
        (test-data (json-read-file "test.json"))
      )
      ; Check $$data_type key is present and has correct value
      (should (string= (gethash "$$data_type" exported-data) "org-document"))
      ; Compare value to test.json
      (json-compare exported-data test-data :cmp-obj #'-json-cmp-exported-objects)
    )
  )
)
