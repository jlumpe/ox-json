# Install dependencies and run tests
# Source: https://github.com/rolandwalker/emacs-travis


EMACS=emacs
EMACS_CLEAN=$(EMACS) --no-site-file
EMACS_BATCH=$(EMACS_CLEAN) --batch
EMACS_PKG=-l package -f package-initialize

WORK_DIR=$(shell pwd)
PACKAGE_NAME=ox-json

TEST_DIR=tests
TEST_DEPS=ert
TEST_FILES=$(notdir $(wildcard $(TEST_DIR)/test-*.el))
# Regex to filter test names
TESTS_REGEXP=
TESTS_EVAL="(ert-run-tests-batch-and-exit '(and \"$(TESTS_REGEXP)\" (not (tag :interactive))))"

EMACS_LIBS=-L $(WORK_DIR) -L $(WORK_DIR)/$(TEST_DIR) $(shell for dep in $(TEST_DEPS); do echo -l $$dep; done)

# This is important, ensures that .emacs.d is in the working directory
HOME := $(WORK_DIR)

# Value of byte-compile-warnings elisp variable
BYTE_COMPILE_WARNINGS='(not docstrings obsolete)


.PHONY : install-deps byte-compile test run-tests test-interactive clean emacs test-deps org-version lint


# Install package and test dependencies
.emacs.d/elpa :
	$(EMACS_CLEAN) --script tests/install-deps.el "$(PACKAGE_NAME)" $(TEST_DEPS) package-lint

# Alias for previous
install-deps : .emacs.d/elpa

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

test : install-deps test-deps run-tests

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
	$(EMACS_CLEAN) $(EMACS_PACKAGE)

clean :
	@rm -f *.elc *~ */*.elc */*~
	@rm -rf .emacs.d/elpa
