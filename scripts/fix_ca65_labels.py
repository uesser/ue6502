#!/usr/bin/env python3
"""Add missing colons to bare ca65 labels without touching instructions.

Usage:
  python3 scripts/fix_ca65_labels.py path/to/file.s
  python3 scripts/fix_ca65_labels.py rom/basic/basic.s rom/basic/min_mon.s
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

OPCODES = {
    "ADC","AND","ASL","BCC","BCS","BEQ","BIT","BMI","BNE","BPL","BRK","BVC","BVS",
    "CLC","CLD","CLI","CLV","CMP","CPX","CPY","DEC","DEX","DEY","EOR","INC","INX",
    "INY","JMP","JSR","LDA","LDX","LDY","LSR","NOP","ORA","PHA","PHP","PLA","PLP",
    "ROL","ROR","RTI","RTS","SBC","SEC","SED","SEI","STA","STX","STY","TAX","TAY",
    "TSX","TXA","TXS","TYA",
}

LABEL_RE = re.compile(r"^(?P<indent>\s*)(?P<label>[A-Za-z_][A-Za-z0-9_]*)(?P<after>\s*)(?P<comment>;.*)?$")
COLON_LINE_RE = re.compile(r"^(?P<indent>\s*)(?P<label>[A-Za-z_][A-Za-z0-9_]*)(?P<after>\s*):(?P<rest>.*)$")


def fix_line(line: str) -> str:
    """Fix bare labels and remove accidental ':' after real instruction mnemonics."""
    if not line.rstrip("\r\n"):
        return line

    text = line.rstrip("\r\n")
    if text.lstrip().startswith(";"):
        return line
    if text.lstrip().startswith(".") or text.lstrip().startswith("*"):
        return line
    if "=" in text and not text.lstrip().startswith("#"):
        return line

    # Remove accidental colon after actual instruction mnemonics like "PHY:" -> "PHY"
    m = COLON_LINE_RE.match(text)
    if m:
        label = m.group("label")
        if label.upper() in OPCODES:
            rest = m.group("rest")
            result = f"{m.group('indent')}{label}{rest}"
            return result + ("\n" if line.endswith("\n") else "")

    m = LABEL_RE.match(text)
    if not m:
        return line

    label = m.group("label")
    if label.upper() in OPCODES:
        return line

    if label.startswith("."):
        return line

    # Catch labels with underscores, e.g. DELAY_LOOP, MINIDLY, ...
    result = f"{m.group('indent')}{label}:{m.group('after')}"
    if m.group("comment"):
        result += m.group("comment")
    return result + ("\n" if line.endswith("\n") else "")


def fix_file(path: Path) -> int:
    text = path.read_text(encoding="utf-8")
    fixed = []
    changed = 0
    for line in text.splitlines(True):
        new_line = fix_line(line)
        if new_line != line:
            changed += 1
        fixed.append(new_line)
    out = "".join(fixed)
    if out != text:
        path.write_text(out, encoding="utf-8")
    return changed


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <file> [<file> ...]", file=sys.stderr)
        return 1

    total = 0
    for arg in sys.argv[1:]:
        path = Path(arg)
        if not path.exists():
            print(f"Missing file: {path}", file=sys.stderr)
            continue
        if path.is_dir():
            for child in sorted(path.rglob("*.s")):
                total += fix_file(child)
        else:
            total += fix_file(path)

    print(f"Fixed {total} label lines.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
