;;; Script to install package and test requirements prior to testing

;; Requirements are extracted from the package file itself
;; Arguments are (PKG-NAME [TEST-DEPS ...])


(require 'cl-lib)
(require 'pcase)
(require 'package)

; Print tracebacks on error
(setq debug-on-error t)


; Print messages prefixed with file name to better tell where they are coming from
(setq msg-prefix (format "[%s] " (file-name-base (or load-file-name buffer-file-name))))

(defun my-message (format-string &rest args)
  (apply 'message (concat msg-prefix format-string) args))

(defun my-message2 (format-string &rest args)
  (apply 'message (format "\n%s********** %s **********" msg-prefix format-string) args))

(my-message2 "Invoked")


; Parse arguments
(setq
  pkg-name (car argv)
  test-deps (mapcar #'intern (cdr argv)))


; Read requirements from package file
(setq
  pkg-file (concat pkg-name ".el")
  pkg-desc (with-temp-buffer
             (insert-file-contents-literally pkg-file)
             (package-buffer-info))
  pkg-requires (package-desc-reqs pkg-desc))


(my-message "pkg-name = %S" pkg-name)
(my-message "test-deps = %S" test-deps)
(my-message "pkg-requires = %S" pkg-requires)


; Configure package system
(my-message2 "Initializing package system")
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/")
  t)
(package-refresh-contents)
(setq package-check-signature nil)


; Install package requirements
(my-message2 "Installing package dependencies")
(let ((transaction (package-compute-transaction nil pkg-requires))
      (byte-compile-warnings nil))
  (package-download-transaction transaction))


; Install test dependencies
(my-message2 "Installing test dependencies")
(dolist (pkg test-deps)
  (if (package-installed-p pkg)
    (my-message "%s is already installed" pkg)
    (my-message "installing %s" pkg)
    (package-install pkg)))


(my-message2 "Done!")
