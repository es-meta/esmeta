#!/usr/bin/env python3
"""Serialize shared modular ITree builds, then exec a bounded parallel make."""

from __future__ import annotations

import fcntl
import os
from pathlib import Path
import sys


def main() -> int:
    if len(sys.argv) != 3:
        print(
            "usage: with-itree-build-lock.py JOBS MAKE_TARGET",
            file=sys.stderr,
        )
        return 2

    try:
        jobs = int(sys.argv[1])
    except ValueError:
        print(f"invalid ITREE_JOBS value: {sys.argv[1]!r}", file=sys.stderr)
        return 2
    if jobs < 1:
        print("ITREE_JOBS must be at least 1", file=sys.stderr)
        return 2

    build_dir = Path("build/itree")
    build_dir.mkdir(parents=True, exist_ok=True)
    lock = (build_dir / ".build.lock").open("a+")
    try:
        fcntl.flock(lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        print("waiting for the active modular ITree build", file=sys.stderr)
        fcntl.flock(lock.fileno(), fcntl.LOCK_EX)

    # Keep the advisory lock alive across exec so it is released only after
    # the bounded parallel make exits.
    os.set_inheritable(lock.fileno(), True)
    os.execvp(
        "make",
        ["make", "--no-print-directory", f"-j{jobs}", sys.argv[2]],
    )
    return 127


if __name__ == "__main__":
    raise SystemExit(main())
