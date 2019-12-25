;;; Script to install package requirements prior to testing

;; Arguments are (PKG-NAME [TEST-DEPS ...])


(require 'cl-lib)
(require 'pcase)
(require 'package)

; Print tracebacks on error
(setq debug-on-error t)


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


(message "pkg-name = %S" pkg-name)
(message "test-deps = %S" test-deps)
(message "pkg-requires = %S" pkg-requires)


; Configure package system
(message "\n********** Initializing package system **********")
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/")
  t)
(package-refresh-contents)
(setq package-check-signature nil)


; Install package requirements
(let ((transaction (package-compute-transaction nil pkg-requires))
      (byte-compile-warnings nil))
  (message "\n********** Installing package dependencies **********" file)
  (package-download-transaction transaction))


; Install test dependencies
(message "\n********** Installing test dependencies **********")
(dolist (pkg test-deps)
  (if (package-installed-p pkg)
    (message "%s is already installed" pkg)
    (message "installing %s" pkg)
    (package-install pkg)))


(message "\nDone!\n\n")
