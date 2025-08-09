; Export test.org to test.json
; Use EXPORT_STRICT=1 to use (:json-strict t)

(require 'ox-json)


(let (
    (ext-plist (list
      :json-strict (equal (getenv "EXPORT_STRICT") "1")
    ))
  )
  (with-current-buffer (find-file-noselect "tests/test.org")
    (org-export-to-file 'json "test.json" nil nil nil nil ext-plist)
  )
)
