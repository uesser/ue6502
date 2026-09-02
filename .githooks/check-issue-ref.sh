#!/bin/bash
# Check that commit message contains an issue reference
# Used as a commit-msg git hook (core.hooksPath = .githooks)
#
# Exceptions: docs and chore commits may omit issue references
# (minor documentation updates, typo fixes per Workflow Decision Guide)

if [ -z "$1" ]; then
    echo "ERROR: No commit message file provided"
    exit 1
fi

# Allow docs/chore commits without issue reference
if grep -qE "^(docs|chore):" "$1"; then
    exit 0
fi

if grep -qE "(refs|closes|Closes) #[0-9]+|#[0-9]+" "$1"; then
    exit 0
else
    echo "ERROR: Commit message must contain issue reference (e.g., refs #1)"
    echo "       Exception: docs: and chore: commits may omit issue references."
    exit 1
fi
