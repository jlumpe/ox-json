;;; test-export.el --- Snapshot tests for full document export

(add-to-list 'load-path (file-name-directory load-file-name))

(require 'ox-json)
(require 'ox-json-test-helpers)


(defvar ox-json-test-export-dir
  (expand-file-name "export"
                    (file-name-directory
                     (or load-file-name
                         (expand-file-name "test-export.el"))))
  "Directory containing .org fixture files and .json snapshots.")


; JSON object keys to ignore in comparison based on $$data_type
; Alist, values are ignore alists to pass to (json-compare)
(setq ignore-by-data-type
  '(
    ("org-document"
      ("properties"
        ("creator" . t)
        ("email" . t)
      )
    )
    ("org-node"
      ; This changes in every export
      ("ref" . t)
      ("target-ref" . t)
      ("properties"
        ; Introduced in org 9.6
        ("mode" . t)
        ; Seems to have a different value in 9.6 vs others?
        ("post-blank" . t)
        ; Added in org 9.7
        ; Not sure if all are useful, some should probably be excluded from export
        ("cached" . t)
        ("deferred" . t)
        ("raw-value" . t)
        ("structure" . t)
        ("secondary" . t)
        ("true-level" . t)
        ; And these seem to have a different value in 9.7...
        ("archivedp" . t)
        ("footnote-section-p" . t)
        ("type-explicit-p" . t)
        ("range-type" . t)
      )
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


(defun ox-json-test-check-export-invariants (data)
  "Check invariants that should hold for every exported JSON document."
  (should (string= (gethash "$$data_type" data) "org-document")))


(defmacro ox-json-def-export-test (name)
  "Define an ERT test that exports NAME.org and compares against NAME.json."
  (let ((org-file (format "%s.org" name))
        (json-file (format "%s.json" name)))
    `(ert-deftest ,(intern (format "test-export-%s" name)) ()
       ,(format "Test that %s exports to match snapshot." org-file)
       (with-json-decode-explicit
         (let* ((ext-plist ox-json-test-export-options)
                (org-path (expand-file-name ,org-file ox-json-test-export-dir))
                (json-path (expand-file-name ,json-file ox-json-test-export-dir))
                (exported-string
                 (with-current-buffer (find-file-noselect org-path)
                   (with-current-buffer (ox-json-export-to-buffer nil nil nil nil ext-plist)
                     (buffer-string))))
                (exported-data (json-read-from-string exported-string))
                (expected-data (json-read-file json-path)))
           (ox-json-test-check-export-invariants exported-data)
           (json-compare exported-data expected-data
                         :cmp-obj #'-json-cmp-exported-objects))))))


(ox-json-def-export-test "headings")
(ox-json-def-export-test "markup")
(ox-json-def-export-test "blocks")
(ox-json-def-export-test "lists")
(ox-json-def-export-test "tables")
(ox-json-def-export-test "links")
(ox-json-def-export-test "timestamps")
(ox-json-def-export-test "footnotes")
(ox-json-def-export-test "latex")
(ox-json-def-export-test "babel")
(ox-json-def-export-test "drawers")
(ox-json-def-export-test "misc")
