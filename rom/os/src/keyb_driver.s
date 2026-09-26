;================================================================================
;   PS/2 keyboard driver through VIA - needs PORTB necessarily
;================================================================================

.include "cpu.inc"

.include "sysdata.inc"
.include "kernelUtils.inc"
.include "via.inc"
.include "lcd.inc"
.include "keyb_driver.h"

.export PS2_DRV_init
.export PS2_DRV_pop_scancode
.export PS2_DRV_pop_scancode_timeout
.export PS2_DRV_set_capslock_led
.export PS2_DRV_set_numlock_led
.export PS2_DRV_set_scrollock_led

.export PS2_DRV_ihandler

KEYB_DDR   = VIA_DDRB
KEYB_PORT  = VIA_PORTB
KEYB_ACR   = VIA_ACR
KEYB_PCR   = VIA_PCR
KEYB_SR    = VIA_SR
KEYB_IER   = VIA_IER
KEYB_IFR   = VIA_IFR
KEYB_T2C_L = VIA_T2C_L
KEYB_T2C_H = VIA_T2C_H

.macro PS2_DRV_PREPARE_READ_CHARACTER
	; Start SR
    ; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
    lda #$20 + $00   ; !!!important!!! first store $20 to ACR, then store $2c to ACR
    sta KEYB_ACR
    lda #$20 + $0c
    sta KEYB_ACR
    lda KEYB_SR      ; also sets back the interrupt flag in IFR
	
	; Set T2 to interrupt after 11 bits
    lda #10
    sta KEYB_T2C_L
    stz KEYB_T2C_H
.endmacro

.macro PS2_DRV_PREPARE_WRITE_CHARACTER
    ; 1. Schieberegister und Timer im ACR komplett abschalten
    ; Das zwingt PB6 und CB2 zu ganz normalen, passiven Digital-Pins
    stz KEYB_ACR
    
    ; 2. Schieberegister blind auslesen, um eventuelle Bit-Reste zu tilgen
    ldx KEYB_SR
.endmacro

.macro PS2_DRV_ADD_TO_BUFFER
    .local katb_add
    .local katb_wr_ptr
    .local katb_end
	; Store a value in the buffer
    ldy ZP_KEYB_WR_PTR
    cpy ZP_KEYB_RD_PTR
    bne katb_add            ; if equal => buffer full

    ; TODO - bell 3 times (audio out)
    bra katb_end
katb_add:
	; Store the character and update the buffer pointer
    sta KEYB_BUFFER, y
    iny
    cpy #<KEYB_BUFFER_SIZE
    bne katb_wr_ptr
    ldy #0
katb_wr_ptr:
    sty ZP_KEYB_WR_PTR
katb_end:
.endmacro	

.macro WAITPB6LOW
    .local wait
wait:
    bit KEYB_PORT
    bvs wait
.endmacro

.macro WAITPB6HIGH
    .local wait
wait:
    bit KEYB_PORT
    bvc wait
    .repeat 5
        nop
    .endrepeat
.endmacro

.segment "OS_CODE"

;================================================================================
;   PS2_DRV_init - initializes the PS2 keyboard driver
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A is Status of init (KB_STATUS_OK or KB_STATUS_ERR)
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
PS2_DRV_init:
    phx

	; init CB1 and CB2
	lda KEYB_PCR
	ora #$10
	and #$1f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 1 => CB1 = Positive Active Edge
	sta KEYB_PCR

	; Set PB6 of KEYB port to input, so clock floats high
	lda KEYB_DDR
	and #%10111111      ; set PB6 to low => PB6 is input
	sta KEYB_DDR

	; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
	lda #$2c
    sta KEYB_ACR

    ; Some USB-compatible keyboards dont act as PS/2 keyboards unless we send a reset command to them first
    ; Furthermore we want to check if keyboard is there and works 
    lda #PS2_RESET
    jsr ps2_drv_write

	; Initialise keyboard buffer pointer WRite ($01) and ReaD ($00). do it here to skip keyb-data (e.g. ACK) at power on.
    ; ReaD is initialized with $00 in function SYSDATA_init.
    lda #1
    sta ZP_KEYB_WR_PTR

    ; Prepare for reading from PS2-Keyboard
    PS2_DRV_PREPARE_READ_CHARACTER

    ; Disable interrupts except for T2 and SR
    lda #$7f
    sta KEYB_IER
    sta KEYB_IFR
    lda #$80 + $24
    sta KEYB_IER

    ; Check auf ACK/BAT $FA $AA

    ; 1. Auf das ACK-Byte (0xFA) warten
    ldx #50                       ; 50 Millisekunden Timeout für das ACK
    jsr PS2_DRV_pop_scancode_timeout
    bcs @init_failed              ; Timeout -> Fehler
    cmp #PS2_ACK
    bne @init_failed              ; Falsches Byte -> Fehler

    ; 2. Auf das BAT-Byte (0xAA) warten
    ldx #250                      ; Erste 250 ms für den Selbsttest warten
    jsr PS2_DRV_pop_scancode_timeout
    bcc @check_bat                ; Byte ist da? Dann direkt prüfen!

    ; Falls die Tastatur etwas träger ist: Weitere 250 ms dranhängen (Gesamt 500 ms)
    ldx #250                      
    jsr PS2_DRV_pop_scancode_timeout
    bcs @init_failed              ; Nach fast einer halben Sekunde immer noch nichts? -> Fehler

@check_bat:
    cmp #PS2_BAT
    bne @init_failed              ; Falsches Byte -> Fehler

    ; Erfolgreich!
    lda #PS2_DRV_STATUS_OK
    bra @init_exit

@init_failed:
    lda #PS2_DRV_STATUS_ERR
@init_exit:
    plx
    rts

; =============================================================================
; PS2_DRV_pop_scancode - gets a scan/keyb code from buffer, if exists.
;
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as scancode if Carry is clear (0)
;                    Carry SET (1): buffer is empty
;   Destroys:        .A, .F
;   ————————————————————————————————————
; =============================================================================
PS2_DRV_pop_scancode:
    phy

    sei                         ; Interrupts sperren für Zeigerkonsistenz
    
    ldy ZP_KEYB_RD_PTR
    iny
    cpy #<KEYB_BUFFER_SIZE
    bne @compare_pointers
    ldy #0

@compare_pointers:
    cpy ZP_KEYB_WR_PTR
    beq @buffer_empty           ; Wenn Lese- und Schreibzeiger gleich -> KEYB_BUFFER ist leer

    ; Byte ist da. Auslesen und Zeiger aktualisieren
    lda KEYB_BUFFER, y
    sty ZP_KEYB_RD_PTR
    
    cli                         ; Interrupts wieder erlauben

    ; Bits umdrehen (da VIA SR und PS/2 gespiegelt arbeiten)
    tay
    lda ps2_scancode_reverse, y ; .A hält nun den echten PS/2 Code (z.B. $FA, $AA (Ack and Basic Assurance Test (BAT)), $1C = 'A', ...)

    ; =======================================================
    ; DIAGNOSE: Jedes ausgelesene Byte sofort im LCD anzeigen
    ; =======================================================
;    jsr LCD_print_hex           ; Zeigt den Wert in .A an (.A bleibt unberührt)
    ; =======================================================

    clc                         ; Carry Clear = Erfolg
    ply
    rts
@buffer_empty:
    cli                         ; Interrupts wieder freigeben
    lda #0
    sec                         ; Carry Set = Puffer leer
    ply
    rts

;================================================================================
;   PS2_DRV_pop_scancode_timeout  - Holt einen Scancode aus dem Puffer mit
;                                flexiblem Timeout
;   ————————————————————————————————————
;   Parameters:      .X = Timeout in Millisekunden (1 bis 255)
;                         (0 bedeutet: Nur einmal kurz prüfen und sofort zurück)
;   Returned Values: .A = Scancode, if Carry Clear (0)
;                         Carry Set (1) = Timeout (no scan code in buffer)
;   Destroys:        .A, .X
;   ————————————————————————————————————
;================================================================================
PS2_DRV_pop_scancode_timeout:
    phy
    
@pgwt_check_buffer:
    jsr PS2_DRV_pop_scancode    ; Schau in den RAM-Puffer
    bcc @pgwt_found              ; Byte da? -> Carry Clear, fertig!

    ; Puffer leer
    cpx #0                       ; Aktuellen Zählerstand auf 0 prüfen
    beq @pgwt_timeout            ; Wenn Zähler auf 0 -> Timeout!
    dex                          ; Zähler für diese Millisekunde verringern

    phx                          ; Zählerstand auf dem Stack sichern
    
    ldx #10                      ; 10 * 100µs = 1000µs = 1ms sleep
    ldy #0
    jsr _kernel_sleep
    plx                          ; Zählerstand in .X wiederherstellen

    bra @pgwt_check_buffer       ; Und wieder von vorn prüfen

@pgwt_found:
    ply
    clc                          ; Erfolg! In .A steht der scancode
    rts

@pgwt_timeout:
    ply
    lda #0
    sec                          ; Fehler / Timeout!
    rts

;================================================================================
;   PS2_DRV_set_capslock_led - set capslock led on/off
;   ————————————————————————————————————
;   Parameters:      .A is 1 => led on, 0 => led off
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
PS2_DRV_set_capslock_led:
    cmp #0
    beq @cl_off
    lda ZP_KEYB_LEDS
    ora #PS2_CAPSLOCK_LED_ON
    bra @cl_do
@cl_off:
    lda ZP_KEYB_LEDS
    and #PS2_CAPSLOCK_LED_OFF
@cl_do:
    jsr ps2_drv_set_leds
    rts

;================================================================================
;   PS2_DRV_set_numlock_led - set numlock led on/off
;   ————————————————————————————————————
;   Parameters:      .A is 1 => led on, 0 => led off
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
PS2_DRV_set_numlock_led:
    cmp #0
    beq @nl_off
    lda ZP_KEYB_LEDS
    ora #PS2_NUMLOCK_LED_ON
    bra @nl_do
@nl_off:
    lda ZP_KEYB_LEDS
    and #PS2_NUMLOCK_LED_OFF
@nl_do:
    jsr ps2_drv_set_leds
    rts

;================================================================================
;   PS2_DRV_set_scrollock_led - set scrollock led on/off
;   ————————————————————————————————————
;   Parameters:      .A is 1 => led on, 0 => led off
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
PS2_DRV_set_scrollock_led:
    cmp #0
    beq @sl_off
    lda ZP_KEYB_LEDS
    ora #PS2_SCROLLOCK_LED_ON
    bra @sl_do
@sl_off:
    lda ZP_KEYB_LEDS
    and #PS2_SCROLLOCK_LED_OFF
@sl_do:
    jsr ps2_drv_set_leds
    rts

;================================================================================
;   ps2_drv_write - Write a byte to the PS/2 port - unbuffered
;   ————————————————————————————————————
;   Parameters:      .A is the byte to be sent to keyboard
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_drv_write:
    phx
    phy

    sei                           ; Disable CPU-Interrupts

    ; Prepare for writing to PS2-Keyboard
    PS2_DRV_PREPARE_WRITE_CHARACTER

    ; Write a byte to the PS/2 port - bitbanging it for now, but it should be possible to use the shift register

    ; Pull clock low, pull data low, let clock go high, wait one tick
    ; Then send data bits one per tick
    ; Then send parity bit and stop bit
    ; Then can read acknowledgement from device

	; Clock low, data low
    stz KEYB_PORT                 ; Prepare output register
    ldx #$40
    stx KEYB_DDR                  ; set PB6 as output -> Clock ist nun LOW

    ldy #0
    ldx #1
    jsr _kernel_sleep             ; Das Protokoll schreibt vor mind. 100µs zu warten bis Data auf LOW gezogen wird
    
    ldx #$ca
    stx KEYB_PCR                  ; set CB2 to low output -> Data ist nun LOW

	ldy #0
    ldx #1
    jsr _kernel_sleep             ; 100µs warten bis Clock (PB6) wieder auf HIGH geht

    ; Let the clock float again (PB6 auf Eingang)
    stz KEYB_DDR                  ; PB6 wieder auf Eingang -> Clock geht HIGH
    
    ; Track odd parity
    ldy #1                        ; 2 cycles

    ; Loop once per bit, least significant bit first (PS/2 order)
    ldx #8                        ; 2 cycles

    clc                           ; 2 cycles; Clear Carry to set it up clean

@ps2_write_bitloop:
;    pha
;    txa
;    jsr LCD_print_hex
;    pla
    ; Send next bit
    ror                           ; 2 cycles; right rotate cause LSB first
    jsr ps2_drv_write_bit         ; 6 cycles
    dex                           ; 2 cycles
    bne @ps2_write_bitloop        ; 3 cyclen (3 branch taken, else 2)

    ; Send the parity bii
    tya                           ; 2 cycles
    ror                           ; 2 cycles
    jsr ps2_drv_write_bit         ; 6 cycles

    ; Send the stop bit
    sec                           ; 2 cycles; set carry = 1
    jsr ps2_drv_write_bit         ; 6 cycles

    ; Wait one more time for the final device clock reading the stop bit
    WAITPB6LOW
    WAITPB6HIGH

    ; Alle während unseres Bit-Bangings entstandenen IFR-Flags löschen
    lda #$24                
    sta KEYB_IFR            

    ; Prepare for reading from PS2-Keyboard
    PS2_DRV_PREPARE_READ_CHARACTER

    cli                           ; CPU-Interrupts wieder freigeben
    ply
    plx
    rts
    
;================================================================================
;   ps2_drv_write_bit - Write a bit to the PS/2 shift register
;   ————————————————————————————————————
;   Parameters:      The bit to write is in the carry flag
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_drv_write_bit:
    pha                           ; 3 cycles

    lda #$ca                      ; Default to pull CB2 low

    bcc @keyb_write_bit_clear     ; low bit 3, high bit 2 cycles (3 branch taken, else 2); bcc = branch on carry clear (carry = 0)

    ; Otherwise track parity and let CB2 float instead
    iny
    lda #$0a                      ; CB2 fluten lassen (High)

@keyb_write_bit_clear:
    ; Wait for PS2-Clock goes LOW to read the Bit we wrote before, then wait for PS2-Clock to go HIGH again, so we can write the next Bit
    WAITPB6LOW
    WAITPB6HIGH

    sta KEYB_PCR                  ; 5 cycles; Send Bit to VIA

    pla                           ; 4 cycles
    rts                           ; 6 cycles

;================================================================================
;   ps2_drv_set_leds - set all leds on/off
;   ————————————————————————————————————
;   Parameters:      .A is led status byte
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_drv_set_leds:
    phx
    phy

    sta ZP_KEYB_LEDS              ; Neuen LED-Zustand zwischenspeichern

    ; Dem Tastatur-Controller Zeit geben seine eben erst beendete scancode Sendung abzuschließen
    ldy #0
    ldx #50                       ; 50 ms warten, bis die Leitungen absolut frei sind
    jsr _kernel_sleep

    ; Befehl $ED (Set LEDs) senden
    lda #PS2_SET_LEDS             ; $ED
    jsr ps2_drv_write

    ; Warten, bis die Tastatur das Befehls-ACK ($FA) geschickt hat

    ldx #50
    jsr PS2_DRV_pop_scancode_timeout
    bcs @led_timeout              ; Timeout? Dann abbrechen.
    cmp #PS2_ACK
    bne @led_err                  ; Falsche Antwort? Abbrechen.

    ; Das LED-Datenbyte hinterhersenden
    lda ZP_KEYB_LEDS              ; Holt den Zustand (z.B. $04 (CapsLock) oder $00 (alle aus))
    jsr ps2_drv_write

    ; Auch das zweite ACK der Tastatur abwarten und verwerfen,
    ; damit es später nicht als "Geister-Taste" im KEYB_BUFFER landet!
    ldx #50
    jsr PS2_DRV_pop_scancode_timeout
    bcs @led_timeout              ; Timeout? Dann abbrechen.
    cmp #PS2_ACK
    bne @led_err                  ; Falsche Antwort? Abbrechen.

    ply
    plx
    rts

@led_timeout:
    lda #$77

@led_err:
;    jsr LCD_print_hex
    ply
    plx
    rts

;================================================================================
;   PS2_DRV_ihandler - PS/2 keyboard IRQ Handler
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
PS2_DRV_ihandler:
    ; Check for PS/2 related VIA interrupts
    lda KEYB_IFR
    and #$24                  ; Timer-2 ($20) or ShiftRegister ($04) interrupt
    bne @irq_via_ps2

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti

@irq_via_ps2:
    ; It's either T2 or SR (shouldn't be both) - check for T2 first
    cmp #$20
    beq @irq_via_ps2_t2

    ; Fall through to handle shift register interrupt

@irq_via_ps2_sr:
    ; Shift register interrupt happens after first 8 bits are read -
    ; that is, a start bit and the first seven data bits    
    lda KEYB_SR

    ; The start bit should have been zero
    bmi irq_via_ps2_framingerror          ; bmi = branch if result negative => highest bit (start bit) is not 0

    sta ZP_KEYB_RD_RESULT

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti

@irq_via_ps2_t2:
    bit KEYB_T2C_L    ; clear interrupt flag in IFR

    ; T2 interrupt happens at the end of the character, read the last few bits, check parity, and add to buffer

    ; Read the SR again
    lda KEYB_SR
    
    ; The bottom bit is the stop bit, which should be set
    ror  ; carry (whatever it is here) is in bit 7 and bit 0 (stop bit) is in carry now
    bcc irq_via_ps2_framingerror    ; if carry is clear (stop bit = 0) => error

    ; Next is parity - then the last data bit.  Add the data bit to the result byte.
    ror  ; stop bit is in bit 7, parity bit is now in carry.
    ror  ; last data bit is now in carry, parity bit is in bit 7.
    rol ZP_KEYB_RD_RESULT  ; Add data bit in carry to the result byte.

    ; The bits of the result byte are now in reverse order - the non-IRQ code can deal with that though

    ; Check the parity - it should be odd
    and #$80
    eor ZP_KEYB_RD_RESULT
    lsr
    eor ZP_KEYB_RD_RESULT
    sta ZP_KEYB_TMP
    lsr
    lsr
    eor ZP_KEYB_TMP
    and #17
    beq irq_via_ps2_framingerror
    cmp #17
    beq irq_via_ps2_framingerror
    
    ; No framing errors, and correct parity, so get ready for the next character, and store this one

    PS2_DRV_PREPARE_READ_CHARACTER

    lda ZP_KEYB_RD_RESULT
    PS2_DRV_ADD_TO_BUFFER

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti

irq_via_ps2_framingerror:
    ; Interrupt the device to resynchronise
    lda KEYB_DDR
	ora #%01000000            ; PB6 as output
    sta KEYB_DDR              ; clock low

	; Wait a while
    ldy #0
    ldx #1                    ; sleep 100us
    jsr _kernel_sleep

	and #%10111111
    sta KEYB_DDR              ; release clock

    ; Prepare for the next character
    PS2_DRV_PREPARE_READ_CHARACTER

    lda #PS2_ERR
    PS2_DRV_ADD_TO_BUFFER

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti


.segment "OS_DATA_RO"

; Due to hardware design, the bits of the PS/2 scancode are in reverse order (comming in via shift register).
; This table reverses them back to normal.
; So, scan code $01 (0000 0001) becomes $80 (1000 0000), scan code $1C (0001 1100) 'Ascii A' becomes $38 (0011 1000), etc.
; Means, if we get a $38 from hardware, we need to look up $38 in this table to get the correct scancode $1C,
; which later is mapped to the correct ASCII value 'A' (see ps2_to_ascii tables below ps2_to_ascii_[lower, upper, altgr]).
ps2_scancode_reverse:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
    .byte $00, $80, $40, $c0, $20, $a0, $60, $e0, $10, $90, $50, $d0, $30, $b0, $70, $f0 ; 0
    .byte $08, $88, $48, $c8, $28, $a8, $68, $e8, $18, $98, $58, $d8, $38, $b8, $78, $f8 ; 1
    .byte $04, $84, $44, $c4, $24, $a4, $64, $e4, $14, $94, $54, $d4, $34, $b4, $74, $f4 ; 2
    .byte $0c, $8c, $4c, $cc, $2c, $ac, $6c, $ec, $1c, $9c, $5c, $dc, $3c, $bc, $7c, $fc ; 3
    .byte $02, $82, $42, $c2, $22, $a2, $62, $e2, $12, $92, $52, $d2, $32, $b2, $72, $f2 ; 4
    .byte $0a, $8a, $4a, $ca, $2a, $aa, $6a, $ea, $1a, $9a, $5a, $da, $3a, $ba, $7a, $fa ; 5
    .byte $06, $86, $46, $c6, $26, $a6, $66, $e6, $16, $96, $56, $d6, $36, $b6, $76, $f6 ; 6
    .byte $0e, $8e, $4e, $ce, $2e, $ae, $6e, $ee, $1e, $9e, $5e, $de, $3e, $be, $7e, $fe ; 7
    .byte $01, $81, $41, $c1, $21, $a1, $61, $e1, $11, $91, $51, $d1, $31, $b1, $71, $f1 ; 8
    .byte $09, $89, $49, $c9, $29, $a9, $69, $e9, $19, $99, $59, $d9, $39, $b9, $79, $f9 ; 9
    .byte $05, $85, $45, $c5, $25, $a5, $65, $e5, $15, $95, $55, $d5, $35, $b5, $75, $f5 ; A
    .byte $0d, $8d, $4d, $cd, $2d, $ad, $6d, $ed, $1d, $9d, $5d, $dd, $3d, $bd, $7d, $fd ; B
    .byte $03, $83, $43, $c3, $23, $a3, $63, $e3, $13, $93, $53, $d3, $33, $b3, $73, $f3 ; C
    .byte $0b, $8b, $4b, $cb, $2b, $ab, $6b, $eb, $1b, $9b, $5b, $db, $3b, $bb, $7b, $fb ; D
    .byte $07, $87, $47, $c7, $27, $a7, $67, $e7, $17, $97, $57, $d7, $37, $b7, $77, $f7 ; E
    .byte $0f, $8f, $4f, $cf, $2f, $af, $6f, $ef, $1f, $9f, $5f, $df, $3f, $bf, $7f, $ff ; F
