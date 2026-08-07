.ifndef _KERNEL_INC_
_KERNEL_INC_ = 1

.setcpu "65C02"
.MACPACK generic

;================================================================================
;
;                                    "UE65c02"
;                                    _________
;
;   kernel: RAM bootloader and viewer (r/o) w/ serial connection support
;
;================================================================================

;--------
; Assumed memory map (based on the UE65c02):
;    $0000 - $7fff      RAM: 32k
;      . $0000 - $00ff      RAM: Zero Page
;      . $0100 - $01ff      RAM: Stack pointer (sp) / Page 1
;      . $0200 - $03ff      RAM: kernel sysRAM / Page 2-3
;      . $0400 - $7fff      RAM: Runnable code area (also see PROGRAM_START/PROGRAM_END)
;    $8010 - $9fff      IO Blk: 8k
;      . $8000 - $83ff      VIA1:
;      . $8400 - $87ff      ACIA:
;      . $8800 - $8bff      SPEAKER:
;    $a000 - $bfff      not decoded
;    $c000 - $ffff      ROM: 16K
;--------

.include "kernel.h"

.endif
