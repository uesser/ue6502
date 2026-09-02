# Project Instructions for AI Assistants

Behavior guidance for AI coding assistants (Claude Code, Codex CLI, Gemini CLI, GitHub Copilot, Warp, Pi) working in this repository.

---

## TL;DR

1. **Just first**: `just --list` shows all commands — prefer Just recipes over manual commands.
2. **Issue first**: Every non-trivial change needs a GitHub issue (via `gh`) — no exceptions.
3. **Branch naming**: `feature/<issue>-<name>`, `fix/<issue>-<name>`, `docs/<issue>-<name>`.
4. **Commits**: `type: description (refs #<issue>)` — never use `closes` in the commit message.
5. **Protected files**: `.env*`, `credentials/`, `**/api-keys.*`, `**/production.*` — always ask the user first.
6. **Language**: All artifacts in English (code, commits, issues, PRs, docs).

---

## General Conventions

### Just Command Runner
- Run `just` or `just --list` to discover available commands.
- The root `justfile` wraps the Make-based build and the GitHub workflow.

### Standard Recipes
- `just build` — build the OS ROM (`rom/os/build/os.bin`)
- `just build-all` — build BASIC + OS images
- `just burn` — write ROM to EPROM via minipro (hardware required)
- `just ci` — run CI checks locally
- `just context` / `just status` — show project context / git status
- `just branch-create` / `just issue-create` / `just pr-create` — GitHub workflow

### Code Quality
- Don't add unnecessary complexity.
- Prefer editing existing files over creating new ones.
- Keep changes focused on the task at hand.

### Protected Files
Require explicit user confirmation before modification: `.env*`, `credentials/`, `secrets/`, `**/api-keys.*`, `**/production.*`, `**/prod.*`, applied hardware/flash images.

---

## Project Overview

**ue6502** is a homebrew 6502-based computer project:

- **ROM OS** written in **65C02 assembly** (`rom/os/src/*.s`), assembled/linked with **ca65/ld65**.
- **ROM BASIC** provided as a **git submodule** (`rom/basic`, external repo `uesser/ue6502-basic`).
- C-style headers for constants / register maps (`rom/os/src/include/*.h`, `*.inc`).
- **Python3** helper scripts (`scripts/`) for ca65 source cleanup.
- **EPROM burning** with `minipro` (AT28C256 hardware).
- VSCode workspace with build tasks (`.vscode/tasks.json`).

**Tech Stack:** 65C02 assembly (ca65/ld65), C headers, Make, Python3, shell, minipro.

**Hosting:** GitHub (`uesser/ue6502`). Use `gh` for issues and pull requests.

---

## Project Structure

```
.
├── justfile                # Just recipes (build, burn, ci, github workflow)
├── rom/
│   ├── os/                 # 65C02 OS ROM sources + Makefile
│   │   ├── src/            #   *.s assembly, include/ headers
│   │   └── ue65c02.cfg     #   ld65 linker config
│   └── basic/              # BASIC ROM (git submodule)
├── scripts/                # Python3 / shell helper scripts
├── .githooks/              # Git hooks (commit-msg issue check)
├── .github/workflows/      # GitHub Actions CI
├── .ai-harness/            # Shared workflow artifacts (catchup, handoff, planning)
├── AGENTS.md               # This file
└── .vscode/                # VSCode tasks
```

---

## Development Workflow

### Workflow Decision Guide

| Intent | Workflow | Branch |
|--------|----------|--------|
| New feature | Issue → Feature branch → PR | `feature/<id>-name` |
| Bug fix | Issue → Feature branch → PR | `fix/<id>-name` |
| Refactoring | Issue → Feature branch → PR | `refactor/<id>-name` |
| Minor docs / typo fix | Direct commit | main |

### 1. Environment Setup
Required tools: `just`, `make`, `ca65`, `ld65`, `python3`, `git`, `gh` (GitHub CLI). Optional: `minipro` for EPROM burning, `pre-commit`.

Initialize the BASIC submodule if missing:

```sh
git submodule update --init --recursive
```

### 2. Implementation
1. Understand the requirement — read the issue, ask clarifying questions.
2. Check existing code — apply the same patterns (assembly include conventions, Make patterns).
3. Implement incrementally — small, focused changes.
4. Stay focused — no scope creep.
5. Update docs if changing behavior or APIs.

### 3. Quality Gate
- `just ci` (build) must pass before committing.
- No secrets in code.
- Review with `git diff --staged`.

---

## GitHub Workflow

Use `gh` for repository operations (issues, pull requests).

1. **Issue first** — check/create a GitHub issue for every change.
2. **Feature branch** — `just branch-create <issue> <name>`.
3. **Implement** — reference issues in commits: `type: description (refs #<issue>)`.
4. **Quality gate** — `just ci` before committing.
5. **Push & PR** — `just pr-create` (uses `gh`).
6. **Review loop** — address review findings, verify the build.
7. **Finalize** — merge PR, then delete the branch.

### Commit & Branch Format
- Commits: `<type>: <description> (refs #<issue>)` — types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`, `ci`. Never use `closes` in commits.
- Branches: `<type>/<issue>-<short-description>`.
