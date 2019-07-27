; Test full export

(require 'ox-json)
(require 'ox-json-test-helpers)


(ert-deftest test-export ()
  "Test export of full org document."
  (let* ((exported-string
            (with-current-buffer (find-file-noselect "test.org")
              (with-current-buffer (ox-json-export-to-buffer)
                (buffer-string))))
          (json-object-type 'hash-table)
          ; For now mostly just test that it is valid JSON:
          (exported-data (json-read-from-string exported-string)))
    ; Check $$data_type key is present and has correct value
    (should (string= (gethash "$$data_type" exported-data) "org-document"))))


(ert-deftest test-get-property-type ()
  ; Test some values from the defaults
  (should (equal (ox-json-get-property-type nil :begin info) nil))
  (should (equal (ox-json-get-property-type nil :post-affiliated info) 'number))
  (should (equal (ox-json-get-property-type 'headline :archivedp info) 'bool))
  )
