;================================================================================
;   LCD interface through VIA - PORTA
;================================================================================

.include "cpu.inc"

.include "constants.inc"
.include "sysram.inc"
.include "kernelUtils.inc"
.include "via.inc"
.include "lcd.h"

.export LCD_init
.export LCD_clear
.export LCD_backspace
.export LCD_newline
.export LCD_print_hex
.export LCD_print_str
.export LCD_print_char

.macro DEC_CURSOR_PTR
    .local dcp_col_zero
    .local dcp_exit
    lda ZP_LCD_COL
    beq dcp_col_zero
    dec
    sta ZP_LCD_COL
    bra dcp_exit
dcp_col_zero:
    ldx ZP_LCD_ROW
    beq dcp_exit
    dex
    stx ZP_LCD_ROW
    lda #LCDCOLS
    dec
    sta ZP_LCD_COL
dcp_exit:
.endmacro

.segment "CODE"

;================================================================================
;   LCD_init - initializes the LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_init:
    pha
	phx
	phy
	
    lda #%11111111 ; Set all pins on port to output
    sta LCD_DDR

    jsr lcd_setup   ; Note: along with delay, this bludgeons A, X and Y (cold and warm reset of LCD)
    
    lda #%00101000 ; Set 4-bit mode; 2-line display; 5x8 font
    jsr lcd_instruction
    lda #%00001110 ; Display on; cursor on; blink off
    jsr lcd_instruction
    lda #%00000110 ; Increment and shift cursor; don't shift display
    jsr lcd_instruction
    lda #%00000001 ; Clear screen
    jsr lcd_instruction
    
    jsr lcd_init_custom_chars   ; initialize custom characters in CGRAM

    jsr LCD_clear

    ply
    plx
	pla
    rts

;================================================================================
;   lcd_setup - setup the LCD using 4 bit interface
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        .A, .X, .Y
;   ————————————————————————————————————
;================================================================================
lcd_setup:
    ; as per Figure 24 (page 46) of the Hitachi data sheet - yes, much like beating it with a rock!
    
    ; delay 50000 us ; > 40ms for Vcc to rise above 2.7V
    ldy #>500
    ldx #<500
    jsr __kernel_sleep
    
    lda #%00000011 ; Set 4-bit mode
    sta LCD_PORT
    ora #E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT
    
    ; delay 4500 us
    ldy #0
    ldx #45
    jsr __kernel_sleep
    
    lda #%00000011 ; Set 4-bit mode
    sta LCD_PORT
    ora #E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT
    
    ; delay 150 us
    ldy #0
    ldx #2
    jsr __kernel_sleep

    lda #%00000011 ; Set 4-bit mode
    sta LCD_PORT
    ora #E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT
    
    ; This 4 bit initialization works well for cold reset (no power to the LCD) but not for resetting
    ; an already initialized and powered up LCD (without power cycling).
    ; More luck with warm reset with even number of 4 bit writes (in case LCD is already in 4 bit mode)
    
    lda #%00000010 ; Set 4-bit mode
    sta LCD_PORT
    ora #E
    sta LCD_PORT
    and #%00001111
    sta LCD_PORT
    
    rts

;================================================================================
;   lcd_init_custom_char - Eigenes Zeichen (z.B. Backslash) ins CGRAM schreiben
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_init_custom_chars:
    pha

    ; 1. Ins CGRAM ab Adresse $40 wechseln
    lda #$48           
    jsr lcd_instruction     

    ; 2. Schleife über alle 56 Bytes (7 Zeichen * 8 Bytes)
    ldx #0                    ; X-Register als Index auf 0 setzen
@loop:
    lda custom_char_data, x   ; Byte aus der Tabelle laden
    jsr lcd_writedata         ; An das LCD senden
    inx                       ; Nächstes Byte
    cpx #56                   ; Haben wir alle 32 Bytes gesendet?
    bne @loop                 ; Wenn nein, Schleife wiederholen

    ; 3. Zurück in den normalen Textmodus (DDRAM) schalten
    lda #$80            
    jsr lcd_instruction

    pla
    rts

;================================================================================
;   lcd_instruction - send command to LCD
;   ————————————————————————————————————
;   Parameters:      .A command
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
lcd_instruction:
    pha

    jsr lcd_wait

    lsr
    lsr
    lsr
    lsr            ; Send high 4 bits
    sta LCD_PORT
    ora #E         ; Set E bit to send instruction
    sta LCD_PORT
    eor #E         ; Clear E bit
    sta LCD_PORT
    pla
    and #%00001111 ; Send low 4 bits
    sta LCD_PORT
    ora #E         ; Set E bit to send instruction
    sta LCD_PORT
    eor #E         ; Clear E bit
    sta LCD_PORT
    rts

;================================================================================
;   lcd_writedata - send data to LCD
;   ————————————————————————————————————
;   Parameters:      .A data
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
lcd_writedata:
    pha

    jsr lcd_wait
    
    lsr
    lsr
    lsr
    lsr             ; Send high 4 bits
    ora #RS         ; Set RS
    sta LCD_PORT
    ora #E          ; Set E bit to send instruction
    sta LCD_PORT
    eor #E          ; Clear E bit
    sta LCD_PORT
    pla
    and #%00001111  ; Send low 4 bits
    ora #RS         ; Set RS
    sta LCD_PORT
    ora #E          ; Set E bit to send instruction
    sta LCD_PORT
    eor #E          ; Clear E bit
    sta LCD_PORT
    rts

;================================================================================
;   lcd_wait - wait till LCD is not busy
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_wait:
    pha
    lda #%11110000     ; LCD data is input
    sta LCD_DDR
@lcdbusy:
    lda #RW
    sta LCD_PORT
    lda #(RW | E)
    sta LCD_PORT
    lda LCD_PORT       ; Read high nibble
    pha                ; and put on stack since it has the busy flag
    lda #RW
    sta LCD_PORT
    lda #(RW | E)
    sta LCD_PORT
    lda LCD_PORT       ; Read low nibble
    pla                ; Get high nibble off stack
    and #%00001000
    bne @lcdbusy

    lda #RW
    sta LCD_PORT
    lda #%11111111     ; LCD data is output
    sta LCD_DDR
    pla
    rts

;================================================================================
;   LCD_clear - clears the LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_clear:
    pha

    ; init lcd buffer
    ldx #0
    lda #$20  ; blank/space char
@init_buf:
    sta LCD_BUFFER, x
    inx
    cpx #LCDMAXCOL
    bne @init_buf

    ; initbuffer index, set cursor to 0,0 and clear screen
    lda #$00
    sta ZP_LCD_BUF_IDX
    sta ZP_LCD_COL
    sta ZP_LCD_ROW
    lda #%00000001 ; Clear screen
    jsr lcd_instruction
    jsr lcd_setcursor
    pla
    rts

;================================================================================
;   lcd_setcursor - sets cursor to ZP_LCD_COL, ZP_LCD_ROW
;   ————————————————————————————————————
;   Parameters:      ZP_LCD_COL, ZP_LCD_ROW
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_setcursor:
    pha
    phx
    ldx ZP_LCD_ROW
    cpx #LCDROWS
    beq @lcdskipsetcursor ; dont wrap around if (col,row) out of range (less confusion)
    
    lda lcdrowstart, x
    adc ZP_LCD_COL
    ora #%10000000 ; Set DDRAM address
    jsr lcd_instruction
@lcdskipsetcursor:
    plx
    pla
    rts

;================================================================================
;   LCD_backspace - sends backspace to LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_backspace:
    pha
    phx
    
    DEC_CURSOR_PTR
    jsr lcd_setcursor
    lda #$20            ; blank
    jsr lcd_writedata
    jsr lcd_setcursor

    plx
    pla
    rts

;================================================================================
;   LCD_newline - sends newline (carriage return) to LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_newline:
    phx
    
    ldx ZP_LCD_ROW
    inx
    cpx #LCDROWS
    bne @lcd_newline_do

    jsr lcd_scroll
    bra @lcd_newline_end
@lcd_newline_do:
    stx ZP_LCD_ROW
    lda lcdbufrowstart, x
    sta ZP_LCD_BUF_IDX
    ldx #$00
    stx ZP_LCD_COL
@lcd_newline_end:
    plx
    jsr lcd_setcursor
    rts

;================================================================================
;   LCD_print_hex - prints data as hexadecimal on LCD
;   ————————————————————————————————————
;   Parameters:      .A byte to print as hex
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_print_hex:
    phx
    pha
    
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda hexmap, x
    jsr LCD_print_char
    pla

    and #$0F
    tax
    lda hexmap, x
    jsr LCD_print_char
    
    pla
    plx
    rts

;================================================================================
;   LCD_print_str - prints a string on LCD
;   ————————————————————————————————————
;   Parameters:      ZP_LCD_STR_PTR, ZP_LCD_STR_PTR+1 pointer to string
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_print_str:
    phy
    pha
    ldy #0
@print_next:
    lda (ZP_LCD_STR_PTR), y
    beq @print_exit
    jsr LCD_print_char
    iny
    bra @print_next
@print_exit:
    pla
    ply
    rts

;================================================================================
;   LCD_print_char - prints a char on LCD
;   ————————————————————————————————————
;   Parameters:      .A char to print
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_print_char:                 ; normaler Aufruf um ein Zeichen auf LCD zu schreiben
    pha
    phx
    
    jsr lcd_write_buf
    
    plx
    pla

lcd_print_char_from_scroll:  ; Aufruf aus lcd_scroll raus
    pha
    phx
    
    jsr lcd_setcursor
    jsr lcd_writedata
    
    ; move cursor to next cell
    inc ZP_LCD_COL
    lda #LCDCOLS
    cmp ZP_LCD_COL
    bne @exit_print_char
    lda #0
    sta ZP_LCD_COL
    inc ZP_LCD_ROW
@exit_print_char:
    jsr lcd_setcursor ; to display next cell position
    
    plx
    pla
    rts

;================================================================================
;   lcd_write_buf - maintains the LCD buffer memory
;   ————————————————————————————————————
;   Parameters:      .A char to put to buffer
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_write_buf:
    phx
    pha

    ldx #LCDMAXCOL
    cpx ZP_LCD_BUF_IDX
    bne @write_buf

    jsr lcd_scroll
@write_buf:
    ldx ZP_LCD_BUF_IDX
    pla
    sta LCD_BUFFER, x  ; store char into LCD_BUFFER
    
    inc ZP_LCD_BUF_IDX
    
    plx
    rts

;================================================================================
;   lcd_scroll - scrolls LCD if necessary
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_scroll:
    phx
    phy
    pha
    
    ; scroll LCD_BUFFER
    ldx #LCD_FIRST_LINE
    ldy #LCD_SECOND_LINE
@memcopy:
    lda LCD_BUFFER, y
    sta LCD_BUFFER, x
    inx
    iny
    cpy #LCDMAXCOL
    bne @memcopy
    ; init last line in buffer
    ldx #LCDMAXSCROLL
    lda #$20  ; blank/space char
@init_line:
    sta LCD_BUFFER, x
    inx
    cpx #LCDMAXCOL
    bne @init_line

    lda #LCDMAXSCROLL
    sta ZP_LCD_BUF_IDX
    
    ; scroll lcd
    jsr LCD_clear
    
    ldx #LCD_FIRST_LINE
@print_chars:
    lda LCD_BUFFER, x
    jsr lcd_print_char_from_scroll
    inx
    cpx #LCDMAXSCROLL
    bne @print_chars
    
    pla
    ply
    plx
    rts


.segment "RODATA"

hexmap: 
    .byte "0123456789ABCDEF"

lcdrowstart:
    .byte $00       ; 20x4 and 16x2
    .byte $40       ; 20x4 and 16x2
    .byte $14       ; 20x4
    .byte $54       ; 20x4

lcdbufrowstart:
    .byte 00
    .byte 20
    .byte 40
    .byte 60

custom_char_data:
    ; Platz $01: Backslash (\)
    .byte $10, $10, $08, $04, $02, $01, $01, $00
    ; Platz $02: Großes Ä
    .byte $0A, $00, $0E, $11, $1F, $11, $11, $00
    ; Platz $03: Großes Ö
    .byte $0A, $00, $0E, $11, $11, $11, $0E, $00
    ; Platz $04: Großes Ü
    .byte $0A, $00, $11, $11, $11, $11, $0E, $00
    ; Platz $05: Euro sign €
    .byte $07, $08, $1E, $08, $1E, $08, $07, $00
    ; Platz $06: Euro sign €
    .byte $06, $08, $04, $0A, $04, $02, $0C, $00
    ; Platz $07: ´
    .byte $01, $02, $04, $00, $00, $00, $00, $00

; Reihe 0:  %00010000  (Hex: $10)   # . . .
; Reihe 1:  %00010000  (Hex: $10)   # . . .
; Reihe 2:  %00001000  (Hex: $08)   . # . .
; Reihe 3:  %00000100  (Hex: $04)   . . # .
; Reihe 4:  %00000010  (Hex: $02)   . . . #
; Reihe 5:  %00000001  (Hex: $01)   . . . . #
; Reihe 6:  %00000001  (Hex: $01)   . . . . #
; Reihe 7:  %00000000  (Hex: $00)   (Cursor-Linie, bleibt meist frei)

; Reihe 0:  %00001010  (Hex: $0A)   . # . # .  (Die Punkte)
; Reihe 1:  %00000000  (Hex: $00)   . . . . .
; Reihe 2:  %00001110  (Hex: $0E)   . # # # .
; Reihe 3:  %00010001  (Hex: $11)   # . . . #
; Reihe 4:  %00011111  (Hex: $1F)   # # # # #
; Reihe 5:  %00010001  (Hex: $11)   # . . . #
; Reihe 6:  %00010001  (Hex: $11)   # . . . #
; Reihe 7:  %00000000  (Hex: $00)   . . . . .

; Reihe 0:  %00001010  (Hex: $0A)   . # . # .
; Reihe 1:  %00000000  (Hex: $00)   . . . . .
; Reihe 2:  %00001110  (Hex: $0E)   . # # # .
; Reihe 3:  %00010001  (Hex: $11)   # . . . #
; Reihe 4:  %00010001  (Hex: $11)   # . . . #
; Reihe 5:  %00010001  (Hex: $11)   # . . . #
; Reihe 6:  %00001110  (Hex: $0E)   . # # # .
; Reihe 7:  %00000000  (Hex: $00)   . . . . .

; Reihe 0:  %00001010  (Hex: $0A)   . # . # .
; Reihe 1:  %00000000  (Hex: $00)   . . . . .
; Reihe 2:  %00010001  (Hex: $11)   # . . . #
; Reihe 3:  %00010001  (Hex: $11)   # . . . #
; Reihe 4:  %00010001  (Hex: $11)   # . . . #
; Reihe 5:  %00010001  (Hex: $11)   # . . . #
; Reihe 6:  %00001110  (Hex: $0E)   . # # # .
; Reihe 7:  %00000000  (Hex: $00)   . . . . .

; Reihe 0:  %00001010  (Hex: $07)   . . # # #
; Reihe 1:  %00000000  (Hex: $08)   . # . . .
; Reihe 2:  %00010001  (Hex: $1E)   # # # # .
; Reihe 3:  %00010001  (Hex: $08)   . # . . .
; Reihe 4:  %00010001  (Hex: $1E)   # # # # .
; Reihe 5:  %00010001  (Hex: $08)   . # . . .
; Reihe 6:  %00001110  (Hex: $07)   . . # # #
; Reihe 7:  %00000000  (Hex: $00)   . . . . .

; Reihe 0:  %00001010  (Hex: $06)   . . # # .
; Reihe 1:  %00000000  (Hex: $08)   . # . . .
; Reihe 2:  %00010001  (Hex: $04)   . . # . .
; Reihe 3:  %00010001  (Hex: $0A)   . # . # .
; Reihe 4:  %00010001  (Hex: $04)   . . # . .
; Reihe 5:  %00010001  (Hex: $02)   . . . # .
; Reihe 6:  %00001110  (Hex: $0C)   . # # . .
; Reihe 7:  %00000000  (Hex: $00)   . . . . .

; Reihe 0:  %00001010  (Hex: $01)   . . . . #
; Reihe 1:  %00000000  (Hex: $02)   . . . # .
; Reihe 2:  %00010001  (Hex: $04)   . . # . .
; Reihe 3:  %00010001  (Hex: $00)   . . . . .
; Reihe 4:  %00010001  (Hex: $00)   . . . . .
; Reihe 5:  %00010001  (Hex: $00)   . . . . .
; Reihe 6:  %00001110  (Hex: $00)   . . . . .
; Reihe 7:  %00000000  (Hex: $00)   . . . . .
