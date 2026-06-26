;;; run-tests.el --- ERT test runner entry point  -*- lexical-binding: t; -*-

(load "test-encode")
(load "test-utils")
(load "test-helpers")
(load "test-export")

(ert-run-tests-batch-and-exit)
