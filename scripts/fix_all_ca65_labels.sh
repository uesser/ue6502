#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
python3 "$ROOT/scripts/fix_ca65_labels.py" "$ROOT/rom" "$ROOT/rom/basic" 2>/dev/null || true

# fall back to walking .s files directly if the above didn't match anything
find "$ROOT" -type f \( -name '*.s' -o -name '*.asm' -o -name '*.a65' \) -print0 | while IFS= read -r -d '' f; do
  python3 "$ROOT/scripts/fix_ca65_labels.py" "$f"
done

echo "ca65 label check finished."
