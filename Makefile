# SPDX-FileCopyrightText: 2025 Campbell Barton
#
# SPDX-License-Identifier: GPL-2.0-or-later

# note: this isn't needed for building,
# its just for some convenience targets.

define HELP_TEXT
Tests:

- test
  Runs all tests.

- watch_test
  Runs all tests, watching files & re-running on change.

Checkers:

- check
  Runs the emacs-batch-check checker.

- watch_check
  Runs the emacs-batch-check checker, watching files & re-running on change.

Environment Variables:

- EMACS_BIN
  The command used to run Emacs, defaults to "emacs"

- EMACS_BATCH_CHECK_BIN
  The command used to check Emacs Lisp, defaults to "emacs-batch-check"
endef
# HELP_TEXT (end)

# Needed for when tests are run from another directory: `make -C ./path/to/tests`.
BASE_DIR := $(CURDIR)

EL_FILES=$(shell find ./ -type f -name '*.el')
PY_FILES=$(shell find ./ -type f -name '*.py')

EXTRA_WATCH_FILES=Makefile

# The test & check targets run the Python scripts as well as the Emacs Lisp,
# leaving either out shows results from before the edit without saying so.
WATCH_FILES=$(EXTRA_WATCH_FILES) $(EL_FILES) $(PY_FILES)

# Exported so `make test EMACS_BIN=...' reaches the test script.
EMACS_BIN?=emacs
export EMACS_BIN

EMACS_BATCH_CHECK_BIN?=$(shell which emacs-batch-check)


# -----------------------------------------------------------------------------
# Help for build targets

export HELP_TEXT
help: FORCE
	@echo "$$HELP_TEXT"


# -----------------------------------------------------------------------------
# Maintenance

docs: FORCE
	@cd "$(BASE_DIR)" && \
	python3 ./_misc/readme_update.py


# -----------------------------------------------------------------------------
# Tests

test: FORCE
	@cd "$(BASE_DIR)" && \
	python3 ./tests/repeat_fu_tests.py

watch_test: require_inotifywait FORCE
	@cd "$(BASE_DIR)" && \
	while true; do \
		$(MAKE) test; \
		inotifywait -q -e close_write $(WATCH_FILES); \
		tput clear; \
	done


# -----------------------------------------------------------------------------
# Checking Utilities

# NOTE: every preset is checked, each with the sibling checkout of the package it
# supports on the load-path.  The presets only declare that package's functions,
# so without it `check-declare' reports every one of them as missing.
#
# NOTE: the test files are left out, they are not a package - `package-lint'
# reports each test name as lacking the file's prefix & the function keys they
# press as reserved.
check: require_emacs_batch_check FORCE
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self repeat-fu.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self repeat-fu-preset-single.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self repeat-fu-preset-multi.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self --load-path=../meep repeat-fu-preset-meep.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self --load-path=../meow repeat-fu-preset-meow.el

watch_check: require_inotifywait FORCE
	@cd "$(BASE_DIR)" && \
	while true; do \
		$(MAKE) check; \
		inotifywait -q -e close_write $(WATCH_FILES); \
		tput clear; \
	done


# -----------------------------------------------------------------------------
# Checker Support

# NOTE: without this check `check' passes having checked nothing.  An unset
# `EMACS_BATCH_CHECK_BIN' leaves each line starting with the "-" of
# "--load-path-self", which make reads as its own ignore-errors modifier - so
# every line fails, every failure is ignored & the target reports success.
require_emacs_batch_check: FORCE
	@test -n "$(EMACS_BATCH_CHECK_BIN)" || { \
		echo "error: emacs-batch-check not found, set EMACS_BATCH_CHECK_BIN to its path" >&2; \
		exit 1; \
	}


# -----------------------------------------------------------------------------
# Watch Support

# NOTE: without this check the watch targets busy-loop.  A missing `inotifywait' fails
# instantly, so the loop re-runs the whole suite continuously pegging a CPU core, and
# `tput clear' wipes the "command not found" before it can be read.
require_inotifywait: FORCE
	@command -v inotifywait >/dev/null || { \
		echo "error: inotifywait not found, install inotify-tools for the watch targets" >&2; \
		exit 1; \
	}


FORCE:
