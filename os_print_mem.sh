#!/bin/bash

# Globale Anweisung für ShellCheck, damit der Parser bei den 
# Regex-Ergebnissen (${BASH_REMATCH}) nicht verwirrt wird
# shellcheck disable=SC2128,SC2178

# 1. Schreibpuffer leeren und kurz warten
sync
sleep 0.1

MAPFILE="$1"

if [ ! -f "$MAPFILE" ]; then
    echo "Fehler: $MAPFILE nicht gefunden!"
    exit 1
fi

if command -v dos2unix >/dev/null 2>&1; then
    dos2unix -q "$MAPFILE"
fi

echo "===================================================================================="
echo " SPEICHERBELEGUNG:"
echo "===================================================================================="
printf "  %-12s  %-8s  %-8s  %-26s  %s\n" "Segment" "Start" "End" "Groesse (Hex / Dezimal)" "Ausrichtung"
echo "  ----------------------------------------------------------------------------------"

# Standardkapazitäten als Fallback
zp_max=256
ram_max=512
rom_max=5888

zp_total=0
ram_total=0
rom_total=0

in_exp=0
in_seg=0

# Explizite Array-Deklarationen für maximale ShellCheck-Konformität
declare -a seg_names=()
declare -a seg_starts=()
declare -a seg_ends=()
declare -a seg_sizes=()

# Zeilenweise Verarbeitung der Datei ohne Altlasten-Variablen
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

# Formatierte Ausgabe
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
        ram_total=$((ram_total + dec))
    elif [[ "$name" =~ ^(CODE|RODATA|VERSDATA|VECTORS)$ ]]; then
        rom_total=$((rom_total + dec))
    fi

    printf "  %-12s  %-8s  %-8s  %-26s  Align: %s\n" "$name" "$start" "$end" "$size_str" "$align_val"
done

echo "  ----------------------------------------------------------------------------------"
printf "  ⚡ ZEROPAGE  : %4d / %4d Bytes belegt (%5.1f%%) -> Noch %4d Bytes FREI\n" "$zp_total" "$zp_max" "$((zp_total * 1000 / zp_max))e-1" "$((zp_max - zp_total))"
printf "  💾 SYSTEM-RAM: %4d / %4d Bytes belegt (%5.1f%%) -> Noch %4d Bytes FREI\n" "$ram_total" "$ram_max" "$((ram_total * 1000 / ram_max))e-1" "$((ram_max - ram_total))"
printf "  💿 ROM GESAMT: %4d / %4d Bytes belegt (%5.1f%%) -> Noch %4d Bytes FREI\n" "$rom_total" "$rom_max" "$((rom_total * 1000 / rom_max))e-1" "$((rom_max - rom_total))"
echo "===================================================================================="
