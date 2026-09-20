;================================================================================
;   LCD interface through VIA - PORTA
;================================================================================

.include "cpu.inc"

.include "ascii.h"
.include "sysram.inc"
.include "kernelUtils.inc"
.include "via.inc"
.include "lcd.h"

.export LCD_init
.export LCD_print_char
.export LCD_print_str
.export LCD_print_hex

; IO
LCD_DDR  = VIA_DDRA
LCD_PORT = VIA_PORTA

; LCD commands
E  = %01000000
RW = %00100000
RS = %00010000

;--------------------------------------------------------------------------------
;   Code
;--------------------------------------------------------------------------------

.segment "CODE"

.macro WRITE_LCD_BUFFER           ; Screibt .A in LCD_BUFFER an die richtige (aktuelle) Stelle - zerstört: NONE
    phx
    
    pha                           ; Zeichen retten
    ldx ZP_LCD_ROW                ; Zeilen-Index (0-3) in X laden
    lda lcd_row_offsets, x        ; je nach Zeilenindex steht jetzt 0, 20, 40 oder 60 in .A
    clc
    adc ZP_LCD_COL                ; in .A steht jetzt die Position an der das Zeichen geschrieben werden muss
    tax                           ; in .X steht jetzt die Position an der das Zeichen geschrieben werden muss
    pla                           ; Zeichen wiederholen
    sta LCD_BUFFER, x

    plx
.endmacro

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

    jsr lcd_setup                 ; Note: along with delay, this bludgeons A, X and Y (cold and warm reset of LCD)
    
    lda #%00101000                ; Set 4-bit mode; 2-line display; 5x8 font
    jsr lcd_instruction
    lda #%00001110                ; Display on; cursor on; blink off
    jsr lcd_instruction
    lda #%00000110                ; Increment and shift cursor; don't shift display
    jsr lcd_instruction
    lda #%00000001                ; Clear screen
    jsr lcd_instruction
    
    jsr lcd_init_custom_chars     ; initialize custom characters in CGRAM

    jsr lcd_clear                 ; clears not only the lcd (like above), but also LCD_BUFFER and initilaizes the cursor variables

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
    ; 1. Ins CGRAM ab Adresse $40 wechseln (ab $40 kann man selbst 8 eigene Zeichen laden ($00-$07), da $00 reserviert ist beginnt es bei $01=$48)
    lda #$48         
    jsr lcd_instruction     

    ; 2. Schleife über alle 56 Bytes (7 Zeichen * 8 Bytes)
    ldx #0                        ; X-Register als Index auf 0 setzen
@loop:
    lda custom_char_data, x       ; Byte aus der Tabelle laden
    jsr lcd_writedata             ; An das LCD senden
    inx                           ; Nächstes Byte
    cpx #custom_char_data_size    ; Haben wir alle xx Bytes gesendet?
    bne @loop                     ; Wenn nein, Schleife wiederholen

    ; 3. Zurück in den normalen Textmodus (DDRAM) schalten
    lda #$80            
    jsr lcd_instruction

    rts

;================================================================================
;   lcd_instruction - send command to LCD
;   ————————————————————————————————————
;   Parameters:      .A command
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_instruction:
    pha
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

    pla
    rts

;================================================================================
;   lcd_writedata - send data to LCD
;   ————————————————————————————————————
;   Parameters:      .A data
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_writedata:
    pha
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

    pla
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
    
    ; 1. Datenleitungen (Pins 0-3) auf EINGANG setzen (0 = Eingang)
    ; Die oberen Pins (4-7) für Steuerleitungen bleiben unberührt.
    lda LCD_DDR
    and #%11110000     ; Pins 0-3 löschen -> werden zu Eingängen
    sta LCD_DDR

@lcdbusy:
    ; --- 1. High-Nibble lesen (enthält das Busy-Flag) ---
    lda #RW            ; RS=0 (Befehl), RW=1 (Read)
    sta LCD_PORT
    lda #(RW | E)      ; Enable HIGH pulsieren
    sta LCD_PORT
    
    lda LCD_PORT       ; Jetzt liegen die oberen 4 Bit des LCDs an Pins 0-3 an
    pha                ; Wert auf dem Stack sichern (enthält BF auf Bit 3)

    lda #RW            ; Enable wieder LOW
    sta LCD_PORT

    ; --- 2. Low-Nibble lesen (muss im 4-Bit-Modus zwingend ausgelesen werden!) ---
    lda #(RW | E)      ; Enable wieder HIGH für das zweite Nibble
    sta LCD_PORT
    
    lda LCD_PORT       ; Low-Nibble einlesen (Inhalt ignorieren wir)
    
    lda #RW            ; Enable wieder LOW
    sta LCD_PORT

    ; --- 3. Busy-Flag auswerten ---
    pla                ; Das gesicherte High-Nibble vom Stack holen
    and #%00001000     ; Maskiert Bit 3 (das Busy-Flag auf deiner Hardware!)
    bne @lcdbusy       ; Wenn Bit 3 noch 1 ist -> LCD ist beschäftigt, nochmal!

    ; 4. Datenleitungen (Pins 0-3) wieder auf AUSGANG setzen (1 = Ausgang)
    lda LCD_DDR
    ora #%00001111     ; Pins 0-3 auf 1 setzen -> wieder Ausgänge
    sta LCD_DDR
    
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
    lda lcdrowstart, x
    clc                           ; !!! vor dem (ersten) adc immer carry löschen !!!
    adc ZP_LCD_COL
    ora #%10000000                ; Set DDRAM address
    jsr lcd_instruction

    plx
    pla
    rts

;================================================================================
;   lcd_clear - clears the LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_clear:
    pha
    phx

    ; 1. Hardware-LCD löschen
    lda #$01                      ; HD44780-Befehl: "Clear Display"
    jsr lcd_instruction    
    
    ; 2. Software-RAM-Puffer mit Leerzeichen ($20) füllen
    lda #ASCII_SPC                ; Leerzeichen (Blank)
    ldx #0
@clear_buffer_loop:
    sta LCD_BUFFER, x
    inx
    cpx #LCDMAXCOL                ; Alle 80 Bytes gelöscht?
    bne @clear_buffer_loop

    ; 3. Cursor-Variablen im RAM zurücksetzen
    stz ZP_LCD_COL
    stz ZP_LCD_ROW
    
    plx
    pla
    rts

;================================================================================
;   lcd_backspace - sends backspace to LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_backspace:
    pha
    
    lda ZP_LCD_COL
    bne @normal_backspace         ; Wenn cursor_x != 0, normales Löschen in der Zeile
    
    ; --- Fall: cursor_x == 0 (Zeilenanfang) ---
    lda ZP_LCD_ROW
    beq @done                     ; Wenn auch cursor_y == 0 ist, sind wir ganz oben links -> Abbruch
    
    ; In die vorherige Zeile wechseln
    dec ZP_LCD_ROW                ; Eine Zeile nach oben springen
    lda #LCDCOLS-1
    sta ZP_LCD_COL                ; Ganz nach rechts springen (Spalte 20, Index 19)
    bra @delete_char              ; Zeichen an dieser neuen Position löschen

@normal_backspace:
    ; --- Fall: Normales Löschen innerhalb der Zeile ---
    dec ZP_LCD_COL                ; Cursor ein Zeichen nach links bewegen

@delete_char:
    ; 1. RAM-Puffer-Adresse berechnen und mit Leerzeichen überschreiben
    lda #ASCII_SPC                ; Leerzeichen (Blank)
    WRITE_LCD_BUFFER
    ; 2. Hardware-Display aktualisieren
    jsr lcd_setcursor             ; LCD-Cursor auf die neue Position (cursor_x/y) setzen
;    lda #ASCII_SPC   ; .A sollte nicht verändert worden sein durch WR_BUFFER und lcd_setcursor
    jsr lcd_writedata             ; Zeichen auf dem LCD mit Blank überschreiben
    
    ; 3. Cursor wieder zurücksetzen, da 'lcd_data' den LCD-Hardware-Cursor automatisch eins nach rechts 
    ; geschoben hat, müssen wir ihn erneut auf unsere Wunschposition zwingen.
    jsr lcd_setcursor

@done:
    pla
    rts

;================================================================================
;   lcd_newline - sends newline (carriage return) to LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_newline:
    pha
    
    ; Wenn in Zeile 3 -> Scrollen, ansonsten nur Zeile inkrementieren und X=0
    lda ZP_LCD_ROW
    cmp #LCDROWS-1
    beq @do_scroll
    inc ZP_LCD_ROW
    stz ZP_LCD_COL
    jsr lcd_setcursor
    pla
    rts
@do_scroll:
    jsr lcd_scroll
    pla
    rts

;================================================================================
;   lcd_tab - prints a tab on LCD
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_tab:
    pha
    lda #ASCII_SPC                ; Leerzeichen (blank)
    jsr lcd_putchar
    jsr lcd_putchar
    pla
    rts

;================================================================================
;   lcd_print_buf - prints the lcd buffer to lcd
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_print_buf:
    pha
    phx

    ldx #0                      ; X ist unser Index im LCD_BUFFER (0 bis 79)

    ; --- ZEILE 0 ---
    stz ZP_LCD_ROW
    stz ZP_LCD_COL
    jsr lcd_setcursor
@loop_row0:
    lda LCD_BUFFER, x           ; Zeichen laden
    jsr lcd_writedata
    inx
    cpx #LCDCOLS                ; Die ersten 20 Zeichen fertig?
    bne @loop_row0

    ; --- ZEILE 1 ---
    lda #1                      ; Zeile 1
    sta ZP_LCD_ROW
    stz ZP_LCD_COL
    jsr lcd_setcursor
@loop_row1:
    lda LCD_BUFFER, x
    jsr lcd_writedata
    inx
    cpx #LCDCOLS * 2            ; Die nächsten 20 Zeichen fertig?
    bne @loop_row1

    ; --- ZEILE 2 ---
    lda #2                      ; Zeile 2
    sta ZP_LCD_ROW
    stz ZP_LCD_COL
    jsr lcd_setcursor
@loop_row2:
    lda LCD_BUFFER, x
    jsr lcd_writedata
    inx
    cpx #LCDCOLS * 3            ; Die nächsten 20 Zeichen fertig?
    bne @loop_row2

    ; --- ZEILE 3 ---
    lda #3                      ; Zeile 3
    sta ZP_LCD_ROW
    stz ZP_LCD_COL
    jsr lcd_setcursor
@loop_row3:
    lda LCD_BUFFER, x
    jsr lcd_writedata
    inx
    cpx #LCDMAXCOL              ; Gesamter Puffer (80 Zeichen) fertig?
    bne @loop_row3

    plx
    pla
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
    pha
    phx
    
    ; 1. Die ersten 3 Zeilen im RAM hochschieben (60 Bytes kopieren)
    ldx #0
@move_loop:
    lda LCD_BUFFER+LCDCOLS, x
    sta LCD_BUFFER, x
    inx
    cpx #LCDMAXSCROLL
    bne @move_loop

    ; 2. Die letzte Zeile (Zeile 3) im RAM mit Leerzeichen füllen (20 Bytes)
@clear_loop:
    lda #ASCII_SPC             ; Leerzeichen (Blank)
    sta LCD_BUFFER, x
    inx
    cpx #LCDMAXCOL
    bne @clear_loop

    ; 3. Das komplette Hardware-LCD aus dem Puffer neu zeichnen
    jsr lcd_print_buf
    
    ; 4. Cursor auf den Anfang der letzten Zeile setzen
    stz ZP_LCD_COL
    lda #LCDROWS-1
    sta ZP_LCD_ROW
    jsr lcd_setcursor
    
    plx
    pla
    rts

;================================================================================
;   lcd_putchar - Put character to LCD_BUFFER and to LCD
;   ————————————————————————————————————
;   Parameters:      .A char to put
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
lcd_putchar:
    WRITE_LCD_BUFFER
    jsr lcd_writedata             ; Auf echtes LCD ausgeben

    pha

    ; Prüfen, ob wir am Zeilenende (Spalte 19) angekommen sind
    lda ZP_LCD_COL
    cmp #LCDCOLS-1
    beq @lp_row_overflow
    inc ZP_LCD_COL                ; Cursor auf dem Papier eins weiter
    pla
    rts

@lp_row_overflow:
    stz ZP_LCD_COL
    inc ZP_LCD_ROW
    lda ZP_LCD_ROW
    cmp #LCDROWS                  ; Letzte Zeile überschritten?
    beq @lp_scroll
    jsr lcd_setcursor             ; Cursor auch auf dem LCD selbst setzen
    pla
    rts

@lp_scroll:
    jsr lcd_scroll                ; Scrollen auslösen
    pla
    rts

;--------------------------------------------------------------------------------
;   Global Methods
;--------------------------------------------------------------------------------

;================================================================================
;   LCD_print_char - prints a char on LCD
;   ————————————————————————————————————
;   Parameters:      .A char to print
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_print_char:
    phx
    phy

    ; Jump Table für Sonderzeichen parsen
    ldy #0
@loop:
    ldx lcd_jump_table, y
    beq @found_match              ; Bei $00 sind wir am Ende -> Fallback auf normale Zeichen
    cmp lcd_jump_table, y
    beq @found_match
    iny
    iny
    iny                           ; 3 Bytes weiter (1 Byte Scancode + 2 Bytes Target Address)
    bra @loop

@found_match:
    pha                           ; Zeichen sichern
    iny                           ; Zeigt auf Low-Byte der Adresse
    lda lcd_jump_table, y
    sta ZP_LCD_JMP_PTR                   
    iny                           ; Zeigt auf High-Byte der Adresse
    lda lcd_jump_table, y
    sta ZP_LCD_JMP_PTR_HI
    pla                           ; Zeichen wiederherstellen
    ply
    plx
    jmp (ZP_LCD_JMP_PTR)          ; Indirekter Sprung zum Handler

;================================================================================
;   LCD_print_str - prints a string on LCD
;   ————————————————————————————————————
;   Parameters:      ZP_LCD_STR_PTR, ZP_LCD_STR_PTR+1 pointer to string
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
LCD_print_str:
    pha
    phy
    ldy #0
@print_next:
    lda (ZP_LCD_STR_PTR), y
    beq @print_exit
    jsr LCD_print_char
    iny
    bra @print_next
@print_exit:
    ply
    pla
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
    pha
    phx
    
    pha
    lda #$24                      ; $24 = dollar sign '$''
    jsr lcd_putchar               ; LCD_print_hex puts ordinary chars only, sono need to call LCD_print_char
    pla
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda hexmap, x
    jsr lcd_putchar
    pla

    and #$0F
    tax
    lda hexmap, x
    jsr lcd_putchar
    
    plx
    pla
    rts

;--------------------------------------------------------------------------------
;   Data declarations
;--------------------------------------------------------------------------------

.segment "RODATA"

lcdrowstart:
    .byte $00       ; 20x4 and 16x2 displays
    .byte $40       ; 20x4 and 16x2 displays
    .byte $14       ; 20x4          display
    .byte $54       ; 20x4          display

lcd_row_offsets:
    .byte 0, 20, 40, 60

; Jump table for special keys like shift, ctrl, alt, altGr, Caps_Lock
lcd_jump_table:
    .byte ASCII_BS
    .word lcd_backspace
    .byte ASCII_HT
    .word lcd_tab
    .byte ASCII_FF
    .word lcd_clear
    .byte ASCII_CR
    .word lcd_newline
    .byte $00               
    .word lcd_putchar             ; Der sichere Ausgang für normale Zeichen

hexmap: 
    .byte "0123456789ABCDEF"

custom_char_data:
    ; Platz $01: Backslash \
    .byte $10, $10, $08, $04, $02, $01, $01, $00
    ; Platz $02: Großes Ä
    .byte $0A, $00, $0E, $11, $1F, $11, $11, $00
    ; Platz $03: Großes Ö
    .byte $0A, $00, $0E, $11, $11, $11, $0E, $00
    ; Platz $04: Großes Ü
    .byte $0A, $00, $11, $11, $11, $11, $0E, $00
    ; Platz $05: Euro sign €
    .byte $07, $08, $1E, $08, $1E, $08, $07, $00
    ; Platz $06: Section sign §
    .byte $06, $08, $04, $0A, $04, $02, $0C, $00
    ; Platz $07: Reverse quote sign ´
    .byte $01, $02, $04, $00, $00, $00, $00, $00

custom_char_data_size = * - custom_char_data

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
