.include "cpu.inc"

.import __RAMLOW_START__
.import __RAMLOW_SIZE__

.export SYSRAM_init                 ; segment .code: initialisiert Zeropage und Sysram mit $00

.exportzp ZP_LCD_COL
.exportzp ZP_LCD_ROW
.exportzp ZP_LCD_JMP_PTR
.exportzp ZP_LCD_JMP_PTR_HI
.exportzp ZP_LCD_STR_PTR
.exportzp ZP_LCD_STR_PTR_HI

.exportzp ZP_KEYB_TMP
.exportzp ZP_KEYB_RD_RESULT
.exportzp ZP_KEYB_WR_PTR
.exportzp ZP_KEYB_RD_PTR
.exportzp ZP_KEYB_JMP_PTR
.exportzp ZP_KEYB_JMP_PTR_HI
.exportzp ZP_KEYB_LEDS
.exportzp ZP_KEYB_FLAGS
.exportzp ZP_KEYB_SCROLL

.exportzp ZP_ACIA_WR_PTR
.exportzp ZP_ACIA_RD_PTR
.exportzp ZP_ACIA_STR_PTR
.exportzp ZP_ACIA_STR_PTR_HI

.exportzp ZP_VIA_TIMER_INT_CNT

.exportzp ZP_KERNEL_LAST_KEY
.exportzp ZP_KERNEL_TAB_WIDTH

.export ACIA_BUFFER_SIZE
.export KEYB_BUFFER_SIZE
.export LCD_BUFFER_SIZE

.export ACIA_BUFFER
.export KEYB_BUFFER
.export LCD_BUFFER

.export shell_cmd_id
.export shell_cmd_tmp
.export shell_buffer_used
.export shell_buffer

.export osversion
.export copywrite

;===================================================================
.segment "ZEROPAGE"

; Used to initialize SYSRAM with $00
ZP_CLEAR_PTR:          .res 1           ; Pointer zum Initialisieren des SYSRAM mit $00
ZP_CLEAR_PTR_HI:       .res 1

; LCD
ZP_LCD_COL:            .res 1           ; current LCD col
ZP_LCD_ROW:            .res 1           ; current LCD row
ZP_LCD_JMP_PTR:        .res 1           ; pointer for jump table lcd_special_table
ZP_LCD_JMP_PTR_HI:     .res 1
ZP_LCD_STR_PTR:        .res 1           ; 2 byte pointer points to 0 terminated string to print
ZP_LCD_STR_PTR_HI:     .res 1

; Keyboard
ZP_KEYB_TMP:           .res 1
ZP_KEYB_RD_RESULT:     .res 1
ZP_KEYB_WR_PTR:        .res 1
ZP_KEYB_RD_PTR:        .res 1
ZP_KEYB_JMP_PTR:       .res 1           ; pointer for jump table ps2_control_table
ZP_KEYB_JMP_PTR_HI:    .res 1
ZP_KEYB_LEDS:          .res 1           ; leds: 1 = scroll lock, 2 = num lock, 4 = caps lock, 8 = 0, 16 = 0, 32 = 0, 64 = 0, 128 = 0
ZP_KEYB_FLAGS:         .res 1           ; keyboard flags: 1 = release, 2 = capsLock, 4 = shift, 8 = ctrl, 16 = altgr, 32 = alt, 64 = fn, 128 = special ($e0)
ZP_KEYB_SCROLL:        .res 1           ; indicator if SCROLL key was pressed (1) or released (0)

; ACIA (RS232)
ZP_ACIA_WR_PTR:        .res 1
ZP_ACIA_RD_PTR:        .res 1
ZP_ACIA_STR_PTR:       .res 1           ; String pointer - ACIA/TTY I/O
ZP_ACIA_STR_PTR_HI:    .res 1

; VIA
ZP_VIA_TIMER_INT_CNT:  .res 1           ; memory address to store how often the via timer interrupt has been called

; KERNEL
ZP_KERNEL_LAST_KEY:    .res 1
ZP_KERNEL_TAB_WIDTH:   .res 1

;===================================================================

.segment "SYSRAM"

ACIA_BUFFER:           .res $80         ; max size 128 ($80) byte
ACIA_BUFFER_SIZE = * - ACIA_BUFFER

KEYB_BUFFER:           .res $20         ; max size 32 ($20) byte
KEYB_BUFFER_SIZE = * - KEYB_BUFFER

LCD_BUFFER:            .res $50         ; max size 80 ($50) byte
LCD_BUFFER_SIZE  = * - LCD_BUFFER

shell_cmd_id:          .res 1
shell_cmd_tmp:         .res 1
shell_buffer_used:     .res 1
shell_buffer:          .res $40         ; max size 64 ($40) byte

;===================================================================

.segment "VERSDATA"

osversion:             .asciiz "OS Vers. v0.2.4"
copywrite:             .asciiz "Copywrite (c) 2026 Udo Esser. All rights reserved."

;===================================================================

.segment "CODE"

SYSRAM_init:
    pha
    phx
    phy

    ; init zero page with $00
    ldx #0
@clear_zp:
    stz $00, x
    inx
    bne @clear_zp

    ; init SYSRAM with $00
    lda #<__RAMLOW_START__        ; Startadresse in den ZeroPage-Pointer laden
    sta ZP_CLEAR_PTR
    lda #>__RAMLOW_START__     
    sta ZP_CLEAR_PTR_HI

    ; Zähler laden
    ldx #>__RAMLOW_SIZE__         ; Anzahl der vollen 256-Byte-Seiten
    ldy #<__RAMLOW_SIZE__         ; Verbleibende Bytes auf der letzten Teil-Seite

    ; Falls Y (Low-Byte) > 0 ist, müssen wir eine zusätzliche Teil-Seite 
    ; bearbeiten. Wir erhöhen X um 1, damit die X-Schleife diese mitnimmt.
    cpy #0
    beq @size_ready
    inx                           ; Aus z.B. $0250 (X=2) wird X=3 Seiten Gesamtleistung

@size_ready:
    ; Sicherheitscheck: Wenn die Gesamtgröße exakt 0 ist, direkt raus
    cpx #0
    beq @done

    ; Da wir rückwärts von 0 ($100) bis 1 zählen, initialisieren wir Y auf 0.
    ; Ein 'sta (ptr),y' mit Y=0 schreibt das erste Byte der Seite.
    lda #$00
    ldy #0
@loop:
    sta (ZP_CLEAR_PTR), y
    iny
    bne @loop                     ; Läuft von 0 bis 255. Wenn Y wieder 0 wird, ist die Seite fertig.

    ; Eine volle Seite ist gelöscht. Pointer auf die nächste Seite anheben.
    inc ZP_CLEAR_PTR_HI
    
    ; Seitenzähler dekrementieren
    dex
    bne @loop                     ; Wenn X noch nicht 0 ist, nächste Seite löschen

@done:
    ply
    plx
    pla
    rts
