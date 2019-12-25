# Source: https://github.com/rolandwalker/emacs-travis

EMACS=emacs

EMACS_CLEAN=--no-site-file
EMACS_BATCH=$(EMACS_CLEAN) --batch
EMACS_PKG=-l package -f package-initialize
TESTS=

CURL=curl --silent
WORK_DIR=$(shell pwd)
PACKAGE_NAME=ox-json
AUTOLOADS_FILE=$(PACKAGE_NAME)-autoloads.el
TRAVIS_FILE=.travis.yml
TEST_DIR=tests
TEST_DEPS=ert

HOME := $(WORK_DIR)

.PHONY : build install autoloads test-autoloads test-travis \
         test test-interactive clean edit test-deps

.emacs.d/elpa :
	$(EMACS) $(EMACS_CLEAN) --script tests/install-deps.el "$(PACKAGE_NAME)" $(TEST_DEPS)

install-deps : .emacs.d/elpa

build : install-deps
	$(EMACS) $(EMACS_BATCH) $(EMACS_PKG)       \
		--eval                                 \
	    "(progn                                \
	      (setq byte-compile-error-on-warn t)  \
	      (batch-byte-compile))" *.el

test-deps :
	for dep in $(TEST_DEPS); do                          \
	  $(EMACS) $(EMACS_BATCH) $(EMACS_PKG) -l $$dep ||   \
	  (echo "Can't load test dependency $$dep"; exit 1); \
	done

autoloads :
	$(EMACS) $(EMACS_BATCH) --eval                       \
	    "(progn                                          \
	      (setq generated-autoload-file \"$(WORK_DIR)/$(AUTOLOADS_FILE)\") \
	      (update-directory-autoloads \"$(WORK_DIR)\"))"

test-autoloads : autoloads
	@$(EMACS) $(EMACS_BATCH) -L . -l "./$(AUTOLOADS_FILE)"      || \
	 ( echo "failed to load autoloads: $(AUTOLOADS_FILE)" && false )

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

test : install-deps test-deps #test-autoloads
	@cd $(TEST_DIR)                                   &&    \
	(for test_lib in test-*.el; do                          \
	    echo "Running tests in ${test_lib}:";               \
	    $(EMACS) $(EMACS_BATCH) $(EMACS_PKG)                \
	    -L .. -L . -l cl                                    \
	    $$(for dep in $(TEST_DEPS); do echo -l $$dep; done) \
		-l $$test_lib                                       \
	    --eval                                              \
	    "(progn                                             \
	      (fset 'ert--print-backtrace 'ignore)              \
	      (ert-run-tests-batch-and-exit                     \
			'(and \"$(TESTS)\" (not (tag :interactive)))))" \
		|| exit 1; \
	done)

emacs :
	$(EMACS) $(EMACS_CLEAN) \
	-l package -f package-initialize

clean :
	@rm -f $(AUTOLOADS_FILE) *.elc *~ */*.elc */*~
	@rm -rf .emacs.d/elpa
