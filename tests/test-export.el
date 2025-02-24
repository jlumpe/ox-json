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
         (exported-data (json-read-from-string exported-string))
         (expected-data (json-read-file "test.json")))
    (should (json-compare exported-data expected-data nil t))))
