#!/bin/bash

# Global instruction for ShellCheck, tell the parser not being
# confused by Regex-results (${BASH_REMATCH})
# shellcheck disable=SC2128,SC2178

# 1. Empty filesystem buffers and wait a short while (0.1s = 100ms)
sync
sleep 0.1

MAPFILE="$1"

if [ ! -f "$MAPFILE" ]; then
    echo "Error: $MAPFILE not found!"
    exit 1
fi

if command -v dos2unix >/dev/null 2>&1; then
    dos2unix -q "$MAPFILE"
fi

echo "===================================================================================="
echo "  MEMORY USAGE:"
echo "===================================================================================="
printf "  %-15s  %-8s  %-8s  %-26s  %s\n" "Segment" "Start" "End" "Size (Hex / Decimal)" "Alignment"
echo "  ----------------------------------------------------------------------------------"

# Standard capacities as fallback
zp_max=256
sysdata_max=512
ram_max=27648
rom_max=5888

zp_total=0
sysdata_total=0
ram_total=0
rom_total=0

in_exp=0
in_seg=0

# Explicit Array-Declarations fopr maximal ShellCheck-conformity
declare -a seg_names=()
declare -a seg_starts=()
declare -a seg_ends=()
declare -a seg_sizes=()

# Line by line processing the mapfile
while read -r line || [ -n "$line" ]; do
    if [[ "$line" =~ ^"Exports list" ]]; then in_exp=1; in_seg=0; continue; fi
    if [[ "$line" =~ ^"Imports list" ]]; then in_exp=0; fi
    
    if [ "$in_exp" -eq 1 ]; then
        if [[ "$line" =~ __ZP_SIZE__[[:space:]]+([0-9A-FA-z]+) ]]; then zp_max=$((16#${BASH_REMATCH[1]})); fi
        if [[ "$line" =~ __RAMLOW_SIZE__[[:space:]]+([0-9A-FA-z]+) ]]; then ram_max=$((16#${BASH_REMATCH[1]})); fi
        if [[ "$line" =~ __ROM_SIZE__[[:space:]]+([0-9A-FA-z]+) ]]; then rom_max=$((16#${BASH_REMATCH[1]})); fi
    fi

    if [[ "$line" =~ ^"Segment list:" ]]; then in_seg=1; continue; fi
    if [[ "$line" =~ ^"Exports list" ]]; then in_seg=0; fi

    if [ "$in_seg" -eq 1 ]; then
        if [[ "$line" =~ ^[[:space:]]*([A-Z0-9_]+)[[:space:]]+([0-9A-F]+)[[:space:]]+([0-9A-F]+)[[:space:]]+([0-9A-F]+) ]]; then
            s_name="${BASH_REMATCH[1]}"
            if [ "$s_name" = "Name" ] || [ "$s_name" = "Segment" ]; then continue; fi
            
            dec_sz=$((16#${BASH_REMATCH[4]}))
            if [ "$dec_sz" -eq 0 ]; then continue; fi
            
            seg_names+=("$s_name")
            seg_starts+=("${BASH_REMATCH[2]}")
            seg_ends+=("${BASH_REMATCH[3]}")
            seg_sizes+=("${BASH_REMATCH[4]}")
        fi
    fi
done < "$MAPFILE"

# Formatted output
for i in "${!seg_names[@]}"; do
    name="${seg_names[$i]}"
    start="${seg_starts[$i]}"
    end="${seg_ends[$i]}"
    size="${seg_sizes[$i]}"
    
    dec=$((16#$size))
    size_str="$size Bytes (dez: $dec)"
    
    start_dec=$((16#$start))
    
    if [ "$start_dec" -eq 0 ]; then align_val="\$0000"
    elif [ $((start_dec % 256)) -eq 0 ]; then align_val="\$0100"
    elif [ $((start_dec % 16)) -eq 0 ]; then align_val="\$0010"
    elif [ $((start_dec % 2)) -eq 0 ]; then align_val="\$0002"
    else align_val="\$0001"; fi

    if [ "$name" = "ZEROPAGE" ]; then
        zp_total=$((zp_total + dec))
    elif [ "$name" = "BSS" ] || [ "$name" = "DATA" ]; then
        sysdata_total=$((sysdata_total + dec))
    elif [ "$name" = "PRG_CODE" ] || [ "$name" = "PRG_DATA" ]; then
        ram_total=$((ram_total + dec))
    elif [[ "$name" =~ ^(OS_CODE|OS_DATA_RO|OS_DATA_VERSION|OS_VECTORS)$ ]]; then
        rom_total=$((rom_total + dec))
    fi

    printf "  %-15s  %-8s  %-8s  %-26s  Align: %s\n" "$name" "$start" "$end" "$size_str" "$align_val"
done

echo "  ----------------------------------------------------------------------------------"
printf "  ⚡ ZEROPAGE  : %5d / %5d Bytes used (%5.1f%%) -> %5d Bytes FREE\n" "$zp_total" "$zp_max" "$((zp_total * 1000 / zp_max))e-1" "$((zp_max - zp_total))"
printf "  💾 SYSTEM-RAM: %5d / %5d Bytes used (%5.1f%%) -> %5d Bytes FREE\n" "$sysdata_total" "$sysdata_max" "$((sysdata_total * 1000 / sysdata_max))e-1" "$((sysdata_max - sysdata_total))"
printf "  💾 RAM       : %5d / %5d Bytes used (%5.1f%%) -> %5d Bytes FREE\n" "$ram_total" "$ram_max" "$((ram_total * 1000 / ram_max))e-1" "$((ram_max - ram_total))"
printf "  💿 ROM total : %5d / %5d Bytes used (%5.1f%%) -> %5d Bytes FREE\n" "$rom_total" "$rom_max" "$((rom_total * 1000 / rom_max))e-1" "$((rom_max - rom_total))"
echo "===================================================================================="
