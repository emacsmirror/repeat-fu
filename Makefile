# SPDX-FileCopyrightText: 2025 Campbell Barton
#
# SPDX-License-Identifier: GPL-2.0-or-later

# note: this isn't needed for building,
# its just for some convenience targets.

define HELP_TEXT
Checkers:

- check
  Runs the emacs-batch-check checker, optionally watching files & re-running.

Environment Variables:

- EMACS_BATCH_CHECK_BIN
  The command used to run Blender, defaults to "emacs-batch-check"
endef
# HELP_TEXT (end)

# Needed for when tests are run from another directory.
BASE_DIR := ${CURDIR}

EL_FILES=$(shell find ./ -type f -name '*.el')

EXTRA_WATCH_FILES=Makefile

EMACS_BATCH_CHECK_BIN?=$(shell which emacs-batch-check)


# -----------------------------------------------------------------------------
# Help for build targets

export HELP_TEXT
help: FORCE
	@echo "$$HELP_TEXT"


# -----------------------------------------------------------------------------
# Checking Utilities

check: FORCE
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self repeat-fu.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self repeat-fu-preset-single.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self --load-path=../bray --load-path=../bray-ex repeat-fu-preset-bray.el
	@$(EMACS_BATCH_CHECK_BIN) --load-path-self --load-path=../meow repeat-fu-preset-meow.el

watch_check:
	@cd "$(BASE_DIR)" && \
	while true; do \
		$(MAKE) check; \
		inotifywait -q -e close_write $(EXTRA_WATCH_FILES) \
		            $(EL_FILES) \
		tput clear; \
	done


FORCE:
