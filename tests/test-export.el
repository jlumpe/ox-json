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


; JSON object keys to ignore in comparison based on $$data_type and Org version
; Expect the JSON saved in version control to have been exported with the latest Org version, there
; will be some differences if we're using an older version.
; Structure is nested alists:
; * First level keys are Org major/minor version lists, e.g. (9 6), or nil for all versions.
;   Values are merged for entries whose version is greater than or equal to the current Org
;   major/minor version when exporting.
; * Second level keys are $$data_type keys. Values are ignore alists to pass to (json-compare)
(setq ignore-by-data-type-version
  '(
    ; Ignore in all versions
    (nil
      ("org-document"
        ("properties"
          ("creator" . t)
        )
      )
      ("org-node"
        ("properties"
        )
      )
    )
    ((9 6)
      ("org-node"
        ("properties"
          ; Added in org 9.7
          ; Not sure if all are useful, some should probably be excluded from export
          ("cached" . t)
          ("deferred" . t)
          ("raw-value" . t)
          ("structure" . t)
          ("secondary" . t)
          ("true-level" . t)
          ; And these seem to have a different value in 9.7 vs 9.6
          ("mode" . t)
          ("type-explicit-p" . t)
          ("range-type" . t)
        )
      )
    )
    ((9 5)
      ("org-node"
        ; Specific failure in headings.org - 9.6 is case sensitive, 9.5 is not
        ("drawer" . t)
        ("properties"
          ; Seems to have a different value in 9.6 vs others?
          ("post-blank" . t)
          ; Added in 9.6
          ("parameters" . t)  ; In special-block
        )
      )
    )
  ))


(defun -json-org-version-list ()
  "Return current Org major/minor version as a list, e.g. (9 6)."
  (let ((version (version-to-list org-version)))
    (list (nth 0 version) (nth 1 version))))


(defun -json-version-applies-p (entry-version current-version)
  "Return non-nil when ENTRY-VERSION applies on CURRENT-VERSION.
ENTRY-VERSION is nil for all versions, otherwise a major/minor list."
  (or (null entry-version)
      (version-list-<= current-version entry-version)))


(defun -json-alist-delete (key alist)
  (cl-remove-if (lambda (item) (equal key (car item))) alist))


(defun -json-merge-ignore-alists (&rest alists)
  "Deep-merge ignore alists, combining keys from all inputs."
  (cl-loop
    with result = '()
    for alist in alists
    do (dolist (item alist)
         (let* ((key (car item))
                (val (cdr item))
                (existing (alist-get-equal key result)))
           (setq result
                 (if (not existing)
                     (cons item result)
                   (if (or (equal existing t) (equal val t))
                       (cons (cons key t) (-json-alist-delete key result))
                     (if (and (listp existing) (listp val))
                         (cons (cons key (-json-merge-ignore-alists existing val))
                               (-json-alist-delete key result))
                       (cons item (-json-alist-delete key result))))))))
    finally return result))


(defun -json-build-ignore-by-data-type (version-alist)
  "Combine VERSION-ALIST entries that apply to the current Org version."
  (let ((current-version (-json-org-version-list))
        (by-type (make-hash-table :test 'equal)))
    (dolist (version-entry version-alist)
      (let ((entry-version (car version-entry))
            (type-alist (cdr version-entry)))
        (when (-json-version-applies-p entry-version current-version)
          (dolist (type-entry type-alist)
            (let ((data-type (car type-entry))
                  (ignore (cdr type-entry)))
              (puthash data-type
                       (-json-merge-ignore-alists
                        ignore (gethash data-type by-type))
                       by-type))))))
    (let (result)
      (maphash (lambda (key value) (push (cons key value) result)) by-type)
      result)))


; Merge applicable entries from ignore-by-data-type-version based on Org version
(setq ignore-by-data-type
  (-json-build-ignore-by-data-type ignore-by-data-type-version))


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
