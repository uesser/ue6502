.include "sysram.h"

.exportzp ZP_LCD_COL
.exportzp ZP_LCD_ROW
.exportzp ZP_LCD_STR_PTR
.exportzp ZP_LCD_STR_PTR_HI
.exportzp ZP_LCD_BUF_IDX
.exportzp ZP_KEYB_TMP
.exportzp ZP_KEYB_RD_RESULT
.exportzp ZP_KEYB_WR_PTR
.exportzp ZP_KEYB_RD_PTR
.exportzp ZP_KEYB_INIT_RESULT
.exportzp ZP_KEYB_LEDS
.exportzp ZP_KEYB_FLAGS
.exportzp ZP_ACIA_WR_PTR
.exportzp ZP_ACIA_RD_PTR
.exportzp ZP_ACIA_SPTR
.exportzp ZP_VIA_TIMER_INT_CNT

.export ACIA_BUFFER
.export KEYB_BUFFER
.export LCD_BUFFER
.export shell_cmd_id
.export shell_cmd_tmp
.export shell_buffer_used
.export shell_buffer

.export osversion

;===================================================================

.segment "ZEROPAGE"

; LCD
ZP_LCD_COL:            .res 1           ; current LCD col
ZP_LCD_ROW:            .res 1           ; current LCD row
ZP_LCD_STR_PTR:        .res 1           ; 2 byte pointer
ZP_LCD_STR_PTR_HI:     .res 1
ZP_LCD_BUF_IDX:        .res 1

; Keyboard
ZP_KEYB_TMP:           .res 1
ZP_KEYB_RD_RESULT:     .res 1
ZP_KEYB_WR_PTR:        .res 1
ZP_KEYB_RD_PTR:        .res 1
ZP_KEYB_INIT_RESULT:   .res 1
ZP_KEYB_LEDS:          .res 1           ; leds: 1 = scroll lock, 2 = num lock, 4 = caps lock, 8 = 0, 16 = 0, 32 = 0, 64 = 0, 128 = 0
ZP_KEYB_FLAGS:         .res 1           ; keyboard flags: 1 = release, 2 = capsLock, 4 = shift, 8 = ctrl, 16 = altgr, 32 = alt, 64 = fn, 128 = special ($e0)

; ACIA (RS232)
ZP_ACIA_WR_PTR:        .res 1
ZP_ACIA_RD_PTR:        .res 1
ZP_ACIA_SPTR:          .res 2           ; String pointer - ACIA/TTY I/O

; VIA
ZP_VIA_TIMER_INT_CNT:  .res 1           ; memory address to store how often the via timer interrupt has been called

;===================================================================

.segment "SYSRAM"

ACIA_BUFFER:           .res $80         ; max size 128 ($80) byte

KEYB_BUFFER:           .res $80         ; max size 128 ($80) byte

LCD_BUFFER:            .res $50         ; max size 80 ($50) byte

shell_cmd_id:          .res 1
shell_cmd_tmp:         .res 1
shell_buffer_used:     .res 1
shell_buffer:          .res $40         ; max size 64 ($40) byte

;===================================================================

.segment "RODATA"

osversion:             .asciiz "OS version 0.2.2"
