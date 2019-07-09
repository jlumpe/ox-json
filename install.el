;;; Script to install package and requirements prior to testing

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

(message "pkg-name = %S" pkg-name)
(message "test-deps = %S" test-deps)


; Configure package system
(message "\n********** Initializing package system **********")
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/")
  t)
(package-refresh-contents)
(setq package-check-signature nil)


; Install
(let ((file (concat pkg-name ".el"))
      (byte-compile-warnings nil))
  (message "\n********** Installing %s **********" file)
  (package-install-file file))


; Install test dependencies
(message "\n********** Installing test dependencies **********")
(dolist (pkg test-deps)
  (if (package-installed-p pkg)
    (message "%s is already installed" pkg)
    (message "installing %s" pkg)
    (package-install pkg)))


(message "\nDone!\n\n")
