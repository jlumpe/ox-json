;;; update-exports.el --- Re-export all .org fixtures in tests/export/ to .json

(add-to-list 'load-path (file-name-directory load-file-name))

(require 'ox-json)
(require 'ox-json-test-helpers)

(let* ((project-root (or (locate-dominating-file default-directory "ox-json.el")
                         default-directory))
       (dir (expand-file-name "tests/export" project-root)))
  (unless (file-directory-p dir)
    (error "Export directory does not exist: %s" dir))
  (dolist (org-file (directory-files dir t "\\.org$"))
    (let ((json-file (concat (file-name-sans-extension org-file) ".json"))
          (ext-plist ox-json-test-export-options))
      (message "Exporting %s -> %s" org-file json-file)
      (with-current-buffer (find-file-noselect org-file)
        (org-export-to-file 'json json-file nil nil nil nil ext-plist)))))

(message "Done.")
