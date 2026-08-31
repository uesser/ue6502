#!/usr/bin/env python3
from __future__ import annotations

import re
import sys
from pathlib import Path

EXTENSIONS = {".s", ".h", ".inc"}


def is_separator(line: str) -> bool:
    return bool(re.fullmatch(r";=+", line.strip()))


def normalize_block(lines: list[str]) -> list[str]:
    if len(lines) < 2:
        return lines

    cleaned: list[str] = []
    for line in lines:
        stripped = line.strip()

        if stripped == ";":
            continue

        cleaned.append(line)

    return cleaned


def normalize_file(path: Path) -> bool:
    original = path.read_text(encoding="utf-8")
    lines = [line for line in original.splitlines() if line.strip() != ";"]
    normalized: list[str] = []
    i = 0
    changed = False

    while i < len(lines):
        line = lines[i]

        if is_separator(line):
            block = [line]
            j = i + 1
            while j < len(lines):
                block.append(lines[j])
                if is_separator(lines[j]):
                    break
                j += 1

            if j < len(lines):
                # The block ends with the next separator line.
                body = block[:-1]
                cleaned = normalize_block(body)
                normalized.extend(cleaned + [lines[j]])
                i = j + 1
                changed = True
                continue

            # Single separator without closing footer, keep as-is.
            normalized.extend(block)
            i = j
            continue

        normalized.append(line)
        i += 1

    new_text = "\n".join(normalized)
    if original.endswith("\n"):
        new_text += "\n"

    if new_text != original:
        path.write_text(new_text, encoding="utf-8")
        return True

    return changed


def iter_target_files(root: Path):
    for path in sorted(root.rglob("*")):
        if path.is_file() and path.suffix.lower() in EXTENSIONS:
            yield path


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    targets = list(iter_target_files(root))

    changed_files = []
    for path in targets:
        if normalize_file(path):
            changed_files.append(str(path.relative_to(root)))

    if changed_files:
        print("Updated files:")
        for item in changed_files:
            print(f"- {item}")
    else:
        print("No comment blocks needed changes.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
