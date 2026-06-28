# Thin make facade over Eask.  Targets delegate to eask; install-deps keeps the
# stamp-file pattern, installing into .eask/ via `eask install-deps --dev`.

EASK ?= eask

# When set (e.g. 28.2), run eask inside silex/emacs Docker (requires Docker).
# scripts/eask-docker.sh uses -u $(id -u):$(id -g) so files aren't owned by root.
EASK_DOCKER ?=
ifneq ($(EASK_DOCKER),)
  EASK := $(abspath scripts/eask-docker.sh) $(EASK_DOCKER)
endif

# Per-Emacs-version stamp so switching EASK_DOCKER re-runs install-deps.
EASK_STAMP = .eask/.stamp$(if $(EASK_DOCKER),-$(EASK_DOCKER),)

# Regex to filter test names (empty = run all non-interactive tests)
TESTS_REGEXP=

# Set to 1 to enable (:json-strict t) in export-test-org rule
EXPORT_STRICT=0

# If non-empty, install-deps rule is a no-op (for running in container with deps already installed)
NO_INSTALL_DEPS=

TEST_DIR=tests
TEST_FILES=$(notdir $(wildcard $(TEST_DIR)/test-*.el))
PACKAGE_FILES=$(wildcard ox-json*.el)
TESTS_EVAL="(ert-run-tests-batch-and-exit '(and \"$(TESTS_REGEXP)\" (not (tag :interactive))))"

# Suppress selected warning categories while still treating others as errors
BYTE_COMPILE_WARNINGS='(not docstrings obsolete suspicious)

COVERAGE_DIR=coverage
COVERAGE_FILE=$(COVERAGE_DIR)/lcov.info
COVERAGE_HTML=$(COVERAGE_DIR)/html


.PHONY : install-deps byte-compile byte-compile-strict test run-tests test-interactive clean emacs org-version lint checkdoc export-test-org edit-test-org coverage


# Install package and dependencies into .eask/
$(EASK_STAMP) :
	@mkdir -p .eask
	$(EASK) install-deps --dev
	@touch $@

install-deps : $(if $(NO_INSTALL_DEPS),,$(EASK_STAMP))

byte-compile-strict : install-deps
	$(EASK) emacs --batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq byte-compile-warnings $(BYTE_COMPILE_WARNINGS))" \
	  --eval "(mapc (lambda (f) (or (byte-compile-file f) (kill-emacs 1))) (directory-files default-directory nil \"^ox-json.*\\\\.el\\\\'\"))"

byte-compile : install-deps
	$(EASK) compile

test : install-deps run-tests

run-tests : install-deps
ifeq ($(TESTS_REGEXP),)
	$(EASK) test ert $(TEST_DIR)/test-*.el
else
	@cd $(TEST_DIR) \
	&& (for test_file in $(TEST_FILES); do \
	  echo "Running tests in $${test_file}:"; \
	  $(EASK) emacs --batch -l $$test_file --eval $(TESTS_EVAL) \
	  || exit 1; \
	done)
endif

lint : install-deps
	$(EASK) lint package $(PACKAGE_FILES)

checkdoc : install-deps
	$(EASK) lint checkdoc $(PACKAGE_FILES)

org-version : install-deps
	@$(EASK) emacs --batch -l org --eval "(princ (org-version))"

test-interactive : install-deps
	$(EASK) emacs -L $(TEST_DIR) \
	  $$(for file in $(TEST_FILES); do echo -l $(TEST_DIR)/$$file; done)

emacs : install-deps
	$(EASK) emacs -L $(TEST_DIR)

# Collect test coverage using undercover.el and optionally generate HTML via genhtml.
# Install the lcov package (apt install lcov) to get genhtml and enable HTML output.
coverage : install-deps
	@mkdir -p $(COVERAGE_DIR)
	$(EASK) clean elc
	$(EASK) emacs --batch -L $(TEST_DIR) \
	  -l $(TEST_DIR)/run-coverage.el
	@echo ""
	@echo "Coverage data written to $(COVERAGE_FILE)"
	@if command -v genhtml > /dev/null 2>&1; then \
	  genhtml $(COVERAGE_FILE) --output-directory $(COVERAGE_HTML) \
	  && echo "HTML report:  open $(COVERAGE_HTML)/index.html"; \
	else \
	  echo "Tip: install lcov (apt install lcov) to generate an HTML report"; \
	fi

clean :
	$(EASK) clean elc
	@rm -rf .eask $(COVERAGE_DIR)

export-test-org : install-deps
	EXPORT_STRICT=$(EXPORT_STRICT) $(EASK) emacs --batch \
	  --eval '(load-file "tests/export.el")'
