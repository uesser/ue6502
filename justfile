# ue6502 — Justfile
# juststart scaffolding, adapted to the 6502/C/Make/Python stack.
# Requires: just, make, ca65, ld65, minipro, python3, git, gh.

# Show all commands
default:
    @just --list

# =========================================================
# BUILD (ca65/ld65 via Makefiles in rom/)
# =========================================================

# Build the OS ROM image (rom/os/build/os.bin)
build:
    @make -C rom/os

# Build the BASIC submodule image (into rom/basic)
build-basic:
    @make -C rom/basic

# Build both OS and BASIC images
build-all: build-basic build
    @echo "Built OS + BASIC ROM images"

# Burn the OS ROM to an EPROM via minipro (requires AT28C256 hardware)
burn:
    @make -C rom/os burn

# Remove build artifacts
clean:
    @make -C rom/os clean
    @make -C rom/basic clean 2>/dev/null || true

# =========================================================
# TEST / LINT / HTTP-SAFETY
# =========================================================

# No automated test suite yet: at least verify assembly syntax
test:
    @echo "No test suite configured. Building as a syntax check..."
    @make -C rom/os

# Run the Python helper scripts' self-checks (if any) — no-op for now
lint:
    @echo "No linters configured for this project."

# Run all CI checks locally (the same checks as GitHub Actions)
ci: build test
    @echo "CI checks passed."

# =========================================================
# GIT STATUS / CONTEXT
# =========================================================

# Print project + git context
context:
    @echo "## Project"
    @echo "  Stack: ca65/ld65 6502 assembly, C headers, Make, Python3, shell"
    @echo ""
    @echo "## Git Status"
    @git status --short
    @echo ""
    @echo "## Current Branch"
    @git branch --show-current

# Compact git status
status:
    @git status --short --branch

# Show recent commit history
log:
    @git log --oneline -15

# =========================================================
# GITHUB WORKFLOW (issue-first; mirrors TomGitLab juststart)
# =========================================================

# Create a GitHub issue (usage: just issue-create 'title' 'body')
issue-create title body:
    @gh issue create --title "{{title}}" --body "{{body}}"

# Create a feature branch from an issue (usage: just branch-create <issue> <name>)
branch-create issue name:
    @git switch -c "feature/{{issue}}-{{name}}"
    @echo "Branch feature/{{issue}}-{{name}} created from issue #{{issue}}"

# Create a fix branch (usage: just branch-create-fix <issue> <name>)
branch-create-fix issue name:
    @git switch -c "fix/{{issue}}-{{name}}"
    @echo "Branch fix/{{issue}}-{{name}} created from issue #{{issue}}"

# List all local and remote branches
branch-list:
    @git branch -a

# Create a pull request for the current branch (usage: just pr-create 'title' 'desc')
pr-create title desc:
    @git push -u origin "$$(git branch --show-current)"
    @gh pr create --title "{{title}}" --body "{{desc}}"

# =========================================================
# HOOKS / PRE-COMMIT
# =========================================================

# Install git hooks + pre-commit
hooks-install:
    @git config core.hooksPath .githooks
    @if command -v pre-commit >/dev/null 2>&1; then \
        pre-commit install; \
    else \
        echo "pre-commit not installed; skipping (git core.hooksPath set to .githooks)"; \
    fi

# =========================================================
# AI HARNESS / SESSION NOTES
# =========================================================

# Catch up on recent activity (see .ai-harness/workflows/catchup.md)
catchup:
    @echo "Catchup workflow: see .ai-harness/workflows/catchup.md"
    @git status --short --branch
    @git log --oneline -10

# Write / update the session notes file
session-note:
    @echo "Remember to record session notes in .ai-harness/session-notes.md"
