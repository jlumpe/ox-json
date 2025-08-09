; Test full export

(require 'ox-json)

(with-current-buffer (find-file-noselect "tests/test.org")
  (org-export-to-file 'json "test.json"))
