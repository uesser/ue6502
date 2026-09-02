# ue6502

Homebrew 6502-based computer project — a 65C02 ROM operating system plus BASIC.

## Overview

- **ROM OS** : 65C02 assembly (`rom/os/src/*.s`), assembled/linked with **ca65/ld65**.
- **ROM BASIC**: provided as a git submodule (`rom/basic` → `uesser/ue6502-basic`).
- **C-style headers** for registers / constants (`rom/os/src/include/*.h`, `*.inc`).
- **Python3** helper scripts for ca65 source cleanup (`scripts/`).
- **EPROM burning** with `minipro` (AT28C256).

## Requirements

- `just` (Just command runner) — see <https://github.com/casey/just>
- `make`, `ca65`, `ld65` (cc65 toolchain), `python3`, `git`
- optional: `minipro` (EPROM flashing), `pre-commit`, `gh` (GitHub CLI)

## Quick Start

```sh
# initialize the BASIC submodule (once)
git submodule update --init --recursive

# show all available recipes
just --list

# build the OS ROM image (rom/os/build/os.bin)
just build

# run the CI checks locally (build + syntax)
just ci
```

## Build

| Recipe          | Purpose                                              |
|-----------------|------------------------------------------------------|
| `just build`    | Build the OS ROM image (`rom/os/build/os.bin`)       |
| `just build-all`| Build BASIC + OS images                              |
| `just burn`     | Write the ROM to an EPROM via `minipro` (hardware)   |
| `just clean`    | Remove all build artifacts                           |

The build is Make-based (`rom/os/Makefile`), which also drives the `rom/basic`
submodule build. The final 32 KB EPROM image is produced by combining the BASIC
and OS binaries.

## Development

This repository follows an **issue-first** workflow, managed with the Just
command runner. See [`AGENTS.md`](AGENTS.md) for the full conventions.

- `just branch-create <issue> <name>` — start a feature branch from an issue
- `just issue-create '<title>' '<body>'` — create a GitHub issue
- `just pr-create '<title>' '<desc>'` — open a pull request for the current branch
- `just ci` — run the quality gate before committing

Commits reference the issue, e.g. `feat: add feature (refs #1)`. Git commit-msg
hooks and optional pre-commit checks are provided under `.githooks/` /
`.pre-commit-config.yaml` (enable with `just hooks-install`).

## CI

GitHub Actions (`.github/workflows/build.yml`) checks out the BASIC submodule
and builds the OS ROM with cc65 on every push to `main` and on pull requests.

## License

Private / personal project. No license file is present.
