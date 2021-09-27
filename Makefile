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
TEST_FILES=$(wildcard $(TEST_DIR)/test-*.el)
# Regex to filter test names
TESTS_REGEXP=
TESTS_EVAL="(ert-run-tests-batch-and-exit '(and \"$(TESTS_REGEXP)\" (not (tag :interactive))))"

EMACS_LIBS=-L .. -L . -l cl $(shell for dep in $(TEST_DEPS); do echo -l $$dep; done)

HOME := $(WORK_DIR)


.PHONY : build install test test-interactive clean edit test-deps


# Test dependencies installed here
.emacs.d/elpa :
	$(EMACS_CLEAN) --script tests/install-deps.el "$(PACKAGE_NAME)" $(TEST_DEPS)

install-deps : .emacs.d/elpa

# Byte-compile elisp files
build : install-deps
	$(EMACS_BATCH) $(EMACS_PKG) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(batch-byte-compile)" \
	  *.el

test-deps :
	for dep in $(TEST_DEPS); do \
	  $(EMACS_BATCH) $(EMACS_PKG) -l $$dep \
	  || (echo "Can't load test dependency $$dep"; exit 1); \
	done

test : install-deps test-deps run-tests

run-tests :
	@cd $(TEST_DIR) \
	&& $(EMACS_BATCH) $(EMACS_PKG) $(EMACS-LIBS) \
	  --exec '(message "org-version = %s\n" (org-version))' \
	&& (for test_file in $(TEST_FILES); do \
	  echo "Running tests in $${test_file}:"; \
	  $(EMACS_BATCH) $(EMACS_PKG) $(EMACS_LIBS) \
	  -l $$test_file \
	  --eval $(TESTS_EVAL) \
	  || exit 1; \
	done)

# Start interactive session with test files and dependencies loaded
# Run tests with M-x ert
test-interactive : install-deps test-deps
	@cd $(TEST_DIR) \
	&& $(EMACS_CLEAN) $(EMACS_PKG) $(EMACS_LIBS) \
	  $$(for file in $(TEST_FILES); do echo -l $$file; done)

emacs :
	$(EMACS_CLEAN) \
	-l package -f package-initialize

clean :
	@rm -f *.elc *~ */*.elc */*~
	@rm -rf .emacs.d/elpa
