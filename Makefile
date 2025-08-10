# Install dependencies and run tests
# Source: https://github.com/rolandwalker/emacs-travis


WORK_DIR=$(shell pwd)


# Customizable vars:

EMACS=emacs

# Regex to filter test names
TESTS_REGEXP=

# In general use, create separate self-contained .emacs.d in the working directory.
# Can override for debugging.
HOME := $(WORK_DIR)

# Set to 1 to enable (:json-strict t) in export-test-org rule
EXPORT_STRICT=0

# If non-empty, install-deps rule is a no-op (for running in container with deps already installed)
NO_INSTALL_DEPS=


# Utility vars:

EMACS_CLEAN=$(EMACS) --no-site-file
EMACS_BATCH=$(EMACS_CLEAN) --batch
EMACS_PKG=-l package -f package-initialize

PACKAGE_NAME=ox-json

TEST_DIR=tests
TEST_DEPS=ert
TEST_FILES=$(notdir $(wildcard $(TEST_DIR)/test-*.el))
TESTS_EVAL="(ert-run-tests-batch-and-exit '(and \"$(TESTS_REGEXP)\" (not (tag :interactive))))"

EMACS_LIBS=-L $(WORK_DIR) -L $(WORK_DIR)/$(TEST_DIR) $(shell for dep in $(TEST_DEPS); do echo -l $$dep; done)

# Value of byte-compile-warnings elisp variable
BYTE_COMPILE_WARNINGS='(not docstrings obsolete suspicious)


.PHONY : install-deps byte-compile test run-tests test-interactive clean emacs test-deps org-version lint export-test-org


# Install package and test dependencies
.emacs.d/elpa :
	$(EMACS_CLEAN) --script tests/install-deps.el "$(PACKAGE_NAME)" $(TEST_DEPS) package-lint

# Alias for previous (unless SKIP_INSTALL_DEPS)
install-deps : $(if $(NO_INSTALL_DEPS),,.emacs.d/elpa)

# Byte-compile elisp files
byte-compile : install-deps
	@$(EMACS_BATCH) $(EMACS_PKG) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq byte-compile-warnings $(BYTE_COMPILE_WARNINGS))" \
	  --eval "(batch-byte-compile)" \
	  *.el

# Check that test dependences can be loaded
test-deps :
	@for dep in $(TEST_DEPS); do \
	  $(EMACS_BATCH) $(EMACS_PKG) -l $$dep \
	  || (echo "Can't load test dependency $$dep"; exit 1); \
	done

# Install dependencies then run the tests
test : install-deps test-deps run-tests

# Run the actual tests
run-tests :
	@cd $(TEST_DIR) \
	&& (for test_file in $(TEST_FILES); do \
	  echo "Running tests in $${test_file}:"; \
	  $(EMACS_BATCH) $(EMACS_PKG) $(EMACS_LIBS) \
	  -l $$test_file \
	  --eval $(TESTS_EVAL) \
	  || exit 1; \
	done)

# Run package-lint
lint : install-deps
	$(EMACS_BATCH) $(EMACS_PKG) \
	  -l package-lint \
	  -f 'package-lint-batch-and-exit' \
	  $(PACKAGE_NAME).el

checkdoc : install-deps
	$(EMACS_BATCH) $(EMACS_PKG) --script tests/checkdoc-batch.el -- $(PACKAGE_NAME).el

# Print the version of org installed
org-version :
	@$(EMACS_BATCH) $(EMACS_PKG) \
	  -l org \
	  --exec '(princ (org-version))' \

# Start interactive session with test files and dependencies loaded
# Run tests with M-x ert
test-interactive : install-deps test-deps
	@cd $(TEST_DIR) \
	&& $(EMACS_CLEAN) $(EMACS_PKG) $(EMACS_LIBS) \
	  $$(for file in $(TEST_FILES); do echo -l $$file; done)

# Run emacs with same configuration used for tests
emacs :
	$(EMACS_CLEAN) $(EMACS_PKG) $(EMACS_LIBS)

clean :
	@rm -f *.elc *~ */*.elc */*~
	@rm -rf .emacs.d/elpa

# Export the test.org file.
# Apparently --script doesn't interact properly with -l/-L, as I found out after several hours of
# debugging. Use --eval and (load-file) instead.
# I hate emacs so much.
export-test-org : install-deps
	EXPORT_STRICT=$(EXPORT_STRICT) $(EMACS_BATCH) $(EMACS_PKG) $(EMACS_LIBS) \
		--eval '(load-file "tests/export.el")'

# Edit tests/test.org with same configuration used for tests
edit-test-org :
	$(EMACS_CLEAN) $(EMACS_PKG) $(EMACS_LIBS) -l ox-json tests/test.org
