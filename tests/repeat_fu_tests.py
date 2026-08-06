#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

# Launch Emacs in batch mode to run the `repeat-fu' tests.
#
# Run with: python3 tests/repeat_fu_tests.py

import os
import subprocess
import sys


THIS_DIR = os.path.normpath(os.path.abspath(os.path.dirname(__file__)))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, ".."))

EMACS_BIN = os.environ.get("EMACS_BIN") or "emacs"


def run_repeat_fu_tests() -> int:
    cmd = [
        EMACS_BIN,
        # NOTE: "-batch" implies "-q" but still loads the site files,
        # leaving the results dependent on how the machine is set up.
        "-Q",
        "-batch",
        # Ensure a stale `*.elc` is never used.
        "--eval", "(setq load-prefer-newer t)",
        "-l", os.path.join(THIS_DIR, "init.el"),
        "-l", os.path.join(THIS_DIR, "repeat_fu_tests.el"),
        "-f", "repeat_fu_tests-run-all",
    ]
    try:
        return subprocess.call(cmd, cwd=BASE_DIR)
    except FileNotFoundError:
        # Report the missing binary instead of a traceback,
        # the likely cause is EMACS_BIN pointing at something that isn't installed.
        sys.stderr.write("error: unable to execute {:s}\n".format(EMACS_BIN))
        return 1


def main() -> int:
    exit_code = 0
    exit_code |= run_repeat_fu_tests()
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
