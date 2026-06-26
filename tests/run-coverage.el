;;; Run all ERT tests with undercover.el coverage instrumentation.

;; undercover MUST be initialized before ox-json.el is loaded, because it
;; instruments the file at load time via edebug.
(require 'undercover)
(setq undercover-force-coverage t)
(undercover "ox-json.el" "ox-json-core.el" "ox-json-utils.el" "ox-json-encode.el" "ox-json-export.el"
            (:report-format 'lcov)
            (:report-file (expand-file-name "coverage/lcov.info"
                            (locate-dominating-file load-file-name "ox-json.el")))
            (:send-report nil)
            (:merge-report nil))

;; Load all test files.  Each one calls (require 'ox-json), which triggers
;; undercover's instrumentation on the first load.
(load "test-utils")
(load "test-encode")
(load "test-helpers")
(load "test-export")

;; Run all collected ERT tests.
(ert-run-tests-batch-and-exit)
