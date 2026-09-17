;================================================================================
;   PS/2 keyboard interface through VIA - needs PORTB necessarily
;================================================================================

.include "cpu.inc"

.include "constants.inc"
.include "ascii.h"
.include "sysram.inc"
.include "kernelUtils.inc"
.include "via.inc"
.include "lcd.inc"
.include "keyboard.h"

.export KEYB_init
.export KEYB_get_wait
.export KEYB_is_shift
.export KEYB_is_capslock
.export KEYB_is_ctrl
.export KEYB_is_alt
.export KEYB_is_altgr
.export KEYB_is_fn

.export KEYB_ihandler

.macro KEYB_PREPARE_READ_CHARACTER
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

.macro KEYB_ADD_TO_BUFFER
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

.macro WAITPB6HIGH
    .local wait
wait:
    bit KEYB_PORT
    bvc wait
.endmacro

.macro WAITPB6LOW
    .local wait
wait:
    bit KEYB_PORT
    bvs wait
.endmacro

.segment "CODE"

;================================================================================
;   KEYB_init - initializes the PS2 keyboard
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_init:
    ; initialize Keyboard Flags und LEDs
    lda #0
    sta ZP_KEYB_FLAGS
    sta ZP_KEYB_LEDS

	; Initialise input buffer
    lda #1
    sta ZP_KEYB_WR_PTR
    stz ZP_KEYB_RD_PTR

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
    lda #PS2_RESET
    jsr ps2_write

    ; Prepare for the first character
    KEYB_PREPARE_READ_CHARACTER

    ; Disable interrupts except for T2 and SR
    lda #$7f
    sta KEYB_IER
    sta KEYB_IFR
    lda #$80 + $24
    sta KEYB_IER

    ; Check auf ACK/BAT $FA $AA

    ; 1. Auf das ACK-Byte (0xFA) warten
    ldx #50                       ; 50 Millisekunden Timeout für das ACK
    jsr PS2_get_wait_timeout
    bcs @init_failed              ; Timeout -> Fehler
    cmp #PS2_ACK
    bne @init_failed              ; Falsches Byte -> Fehler

    ; 2. Auf das BAT-Byte (0xAA) warten
    ldx #250                      ; Erste 250 ms für den Selbsttest warten
    jsr PS2_get_wait_timeout
    bcc @check_bat                ; Byte ist da? Dann direkt prüfen!

    ; Falls die Tastatur etwas träger ist: Weitere 250 ms dranhängen (Gesamt 500 ms)
    ldx #250                      
    jsr PS2_get_wait_timeout
    bcs @init_failed              ; Nach fast einer halben Sekunde immer noch nichts? -> Fehler

@check_bat:
    cmp #PS2_BAT
    bne @init_failed              ; Falsches Byte -> Fehler

    ; Erfolgreich!
    lda #KB_STATUS_OK
    rts

@init_failed:
    lda #KB_STATUS_ERR
    rts

; =============================================================================
; ps2_pop_scancode - gets a scan/keyb code from buffer, if exists.
;
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as scancode if Carry is clear (0)
;                    Carry SET (1): buffer is empty
;   Destroys:        .A, .F
;   ————————————————————————————————————
; =============================================================================
ps2_pop_scancode:
    phy

    sei                         ; Interrupts sperren für zeigerkonsistenz
    ldy ZP_KEYB_RD_PTR
    iny
    cpy #<KEYB_BUFFER_SIZE
    bne @compare_pointers
    ldy #0

@compare_pointers:
    cpy ZP_KEYB_WR_PTR
    beq @buffer_empty           ; Wenn Lese- und Schreibzeiger gleich -> Leer!

    ; Byte ist da! Auslesen und Zeiger aktualisieren
    lda KEYB_BUFFER, y
    sty ZP_KEYB_RD_PTR
    cli                         ; Interrupts wieder erlauben

    ; Bits umdrehen (da VIA SR und PS/2 gespiegelt arbeiten)
    tay
    lda ps2_scancode_reverse, y ; .A hält nun den echten PS/2 Code (z.B. $FA, $AA (Ack and Basic Assurance Test (BAT)), $1C = 'A')

    ; =======================================================
    ; DIAGNOSE: Jedes ausgelesene Byte sofort im LCD anzeigen
    ; =======================================================
;    jsr LCD_print_hex           ; Zeigt den Wert in .A an (.A bleibt unberührt)
    ; =======================================================

    clc                         ; Carry Clear = Erfolg
    ply
    rts
@buffer_empty:
    lda #0
    sec                         ; Carry Set = Puffer leer
    cli                         ; Interrupts wieder freigeben
    ply
    rts

;================================================================================
;   KEYB_get_wait - Get ASCII from keyboard buffer
;                   waits for next keystroke/scancode
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as ASCII or $00 if special key (ctrl, shift, ...) or
;                       no valid scancode
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_get_wait:
@keyb_wait_for_scancode_wait:	
    jsr ps2_pop_scancode
    bcc @keyb_read_from_buffer_gotchar_wait       ; Carry Clear means got scancode from keyboard buffer

	; The buffer is empty, wait for an interrupt
	wai
	cli
	bra @keyb_wait_for_scancode_wait

@keyb_read_from_buffer_gotchar_wait:
    jsr ps2_to_ascii

	rts

;================================================================================
;   KEYB_get_wait_timeout  - Get ASCII from keyboard buffer
;                            waits for next keystroke till timeout of 10ms
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as ASCII or $00 if special key (ctrl, shift, ...) or
;                       no valid scancode
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_get_wait_timeout:
    plx
    ldx #10                      ; 10 Millisekunden warten
    jsr PS2_get_wait_timeout
    bcs @kgwt_exit
    jsr ps2_to_ascii
@kgwt_exit:
    plx
    rts

;================================================================================
;   PS2_get_wait_timeout  - Holt einen Scancode aus dem Puffer mit
;                           flexiblem Timeout
;   ————————————————————————————————————
;   Parameters:      .X = Timeout in Millisekunden (1 bis 255)
;                         (0 bedeutet: Nur einmal kurz prüfen und sofort zurück)
;   Returned Values: .A = Scancode, if Carry Clear (0)
;                         Carry Set (1) = Timeout (no scan code in buffer)
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
PS2_get_wait_timeout:
    phx
    phy
    
    txa                          ; Timeout-Wert aus .X in .A holen
    pha                          ; Und sicher auf dem Stack parken (Unser Zähler)

@pgwt_check_buffer:
    jsr ps2_pop_scancode         ; Schau in den RAM-Puffer
    bcc @pgwt_found              ; Byte da? -> Carry Clear, fertig!

    ; Wenn Puffer leer, holen wir den Zähler vom Stack, um ihn zu prüfen
    pla                          ; Aktuellen Zählerstand holen
    beq @pgwt_timeout            ; Wenn Zähler auf 0 -> Timeout!
    dec                          ; Zähler für diese Millisekunde verringern
    pha                          ; Neuen Zählerstand sofort wieder auf dem Stack sichern

    ; --- 1 Millisekunde warten via __kernel_sleep ---
    ; Da __kernel_sleep X und Y zerstört, müssen wir sie für jeden Aufruf laden
    ldx #10                      ; 10 * 100µs = 1000µs = 1ms
    ldy #0
    jsr __kernel_sleep

    bra @pgwt_check_buffer       ; Und wieder von vorn prüfen

@pgwt_found:
    ply                          ; Den ungenutzten Zähler vom Stack aufräumen!
    ply
    plx
    clc                          ; Erfolg!
    rts

@pgwt_timeout:
    ; Der Stack ist an dieser Stelle durch das 'pla' oben bereits sauber, braucht also nicht von Stack abgeräumt zu werden!
    ply
    plx
    lda #0
    sec                          ; Fehler / Timeout!
    rts

;================================================================================
;   KEYB_is_shift - returns 0 if shift is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if shift is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_shift:
    lda ZP_KEYB_FLAGS
	and #PS2_SHIFT
    rts

;================================================================================
;   KEYB_is_capslock - returns 0 if capslock is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if capslock is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_capslock:
    lda ZP_KEYB_FLAGS
	and #PS2_CAPSLOCK
    rts

;================================================================================
;   KEYB_is_ctrl - returns 0 if ctrl is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if ctrl is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_ctrl:
    lda ZP_KEYB_FLAGS
	and #PS2_CTRL
    rts

;================================================================================
;   KEYB_is_alt - returns 0 if alt is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if alt is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_alt:
    lda ZP_KEYB_FLAGS
	and #PS2_ALT
    rts

;================================================================================
;   KEYB_is_altgr - returns 0 if altgr is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if altgr is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_altgr:
    lda ZP_KEYB_FLAGS
	and #PS2_ALTGR
    rts

;================================================================================
;   KEYB_is_fn - returns 0 if fn is not set, != 0 else
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if fn is not set, != 0 else
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
KEYB_is_fn:
    lda ZP_KEYB_FLAGS
	and #PS2_FN
    rts

;================================================================================
;   ps2_write - Write a byte to the PS/2 port - unbuffered
;   ————————————————————————————————————
;   Parameters:      .A is the byte to be sent to keyboard
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_write:
    phx
    phy
    ; Write a byte to the PS/2 port - bitbanging it for now, but it should be possible to use the shift register

    ; Pull clock low, pull data low, let clock go high, wait one tick
    ; Then send data bits one per tick
    ; Then send parity bit and stop bit
    ; Then can read acknowledgement from device

	; Clock low, data low
    pha
    lda KEYB_PORT
    and #%10111111
	sta KEYB_PORT       ; set PB6 low
    lda KEYB_DDR
    ora #%01000000
    sta KEYB_DDR        ; set PB6 as output
    lda KEYB_PCR        ; set CB2
	ora #$c0            ; to low output
    sta KEYB_PCR

	; Wait a while
    ldy #0
    ldx #1              ; sleep 100us
    jsr __kernel_sleep

    ; Let the clock float again
    lda KEYB_DDR
    and #%10111111
	sta KEYB_DDR        ; set PB6 as input
    pla

    ; Track odd parity
    ldy #1

    ; Loop once per bit
    ldx #8

@ps2_write_bitloop:
    ; Send next bit
    rol
    jsr ps2_write_bit

    dex
    bne @ps2_write_bitloop

    ; Send the parity bit
    tya                     ; y-register to accumulator
    ror
    jsr ps2_write_bit

    ; Send the stop bit
    sec                     ; set carry = 1
    jsr ps2_write_bit

    ; Wait one more time
    jsr ps2_write_bit

    ply
    plx
    rts
    
;================================================================================
;   ps2_write_bit - Write a bit to the PS/2 shift register
;   ————————————————————————————————————
;   Parameters:      The bit to write is in the carry flag
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
ps2_write_bit:
    pha

    ; Default to pull CB2 low
    lda KEYB_PCR
    ora #$c0

    ; If next bit is clear, that is the right state for CB2
    bcc @ps2_write_bit_clear        ; bcc = branch on carry clear (carry = 0)

    ; Otherwise track parity and let CB2 float instead
    iny
    and #$1f

@ps2_write_bit_clear:
    ; Wait for one tick from the device
    WAITPB6HIGH
    WAITPB6LOW
    
    ; Set the CB2 state
    sta KEYB_PCR

    pla
    rts

;================================================================================
;   ps2_to_ascii - converts PS2-scancode to ASCII
;   ————————————————————————————————————
;   Parameters:      .A is PS2-scancode
;   Returned Values: .A as ASCII or $00 if special key (ctrl, shift, ...) or
;                       no valid scancode
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_to_ascii:
    phx
    phy
    pha                           ; scan code sichern

    ; Nur zum Test: print scancode as hex
    ; jsr LCD_print_hex

    ; Test code, ob bei Drücken 'A' (key code = $1c) ein 'A' ausgegeben wird.	
    ; cmp #$1c                     ; key code $1c = 'A' in ascii
    ; bne @pta_chk_special
    ; tax                          ; swap A to X => to have the index into the lookup tables
    ; lda ps2_to_ascii_upper, X    ; default use ascii_lower
    ; pla
    ; ply
    ; plx
    ; rts

    ; 1. Vorab-Prüfung auf Protokoll-Scancodes ($E0, $F0, Fehler)
    cmp #$e0                      ; Special keys wie AltGr Präfix
    bne @no_special
    lda ZP_KEYB_FLAGS
    ora #PS2_SPECIAL
    sta ZP_KEYB_FLAGS
    jmp pta_return_zero           ; JMP statt BRA wegen Reichweite

@no_special:
    cmp #$f0                      ; Key Release Code Präfix
    bne @no_release
    lda ZP_KEYB_FLAGS
    ora #PS2_RELEASE
    sta ZP_KEYB_FLAGS
    jmp pta_return_zero           ; JMP statt BRA wegen Reichweite

@no_release:
    cmp #$80                      ; < $80 ok, sonst ungültiger Scancode (Framing Error etc.)
    bcc @start_table              ; Carry clear heißt less than (<)
    jmp pta_return_zero           ; Brücke für weiten Sprung

@start_table:
    ; 2. Jump Table für Sondertasten parsen
    pla                           ; geretteten scan code wiederholen
    ldy #0
@loop:
    ldx ps2_control_table, y
    beq @found_match              ; Bei $00 sind wir am Ende -> Fallback auf normale Tasten
    cmp ps2_control_table, y
    beq @found_match
    iny
    iny
    iny                           ; 3 Bytes weiter (1 Byte Scancode + 2 Bytes Target Address)
    bra @loop

@found_match:
    pha                           ; scan code sichern
    iny                           ; Zeigt auf Low-Byte der Adresse
    lda ps2_control_table, y
    sta ZP_KEYB_JMP_PTR                   
    iny                           ; Zeigt auf High-Byte
    lda ps2_control_table, y
    sta ZP_KEYB_JMP_PTR_HI
    jmp (ZP_KEYB_JMP_PTR)         ; Indirekter Sprung zum Handler

;================================================================================
; SPECIAL KEY HANDLERS
;================================================================================

pta_capslock:
    lda ZP_KEYB_FLAGS
    and #PS2_RELEASE              ; Prüfe, ob es ein Loslass-Event ($F0 $58) ist
    beq @pta_capslock_end         ; Wenn 0 (= gedrückt), ignorieren wir das Event völlig!

    ; --- CAPS LOCK RELEASE ($F0 $58) -> HIER TOGGELN WIR ---
    lda ZP_KEYB_FLAGS
    eor #PS2_CAPSLOCK             ; CapsLock Zustand invertieren
    and #PS2_RELEASE_END          ; Release-Bit direkt wieder löschen (Entspricht PS2_RELEASE_END)
    sta ZP_KEYB_FLAGS

    ; LED basierend auf neuem Zustand setzen
    and #PS2_CAPSLOCK
    beq @pta_capslock_led_off
    lda #1                        ; LED an
    bra @pta_capslock_set_led
@pta_capslock_led_off:
    lda #0                        ; LED aus
@pta_capslock_set_led:
    jsr ps2_set_capslock_led
@pta_capslock_end:
    jmp pta_return_zero           ; Gedrückt halten/Wiederholen wird komplett ignoriert


pta_shift:
    lda ZP_KEYB_FLAGS
    and #PS2_RELEASE
    bne @pta_shift_rel
    lda ZP_KEYB_FLAGS
    ora #PS2_SHIFT
    bra @pta_shift_save
@pta_shift_rel:
    lda ZP_KEYB_FLAGS
    and #PS2_SHIFT_END            ; Nutzt deine Bitmaske invertiert zum Löschen
@pta_shift_save:
    sta ZP_KEYB_FLAGS
    jmp pta_return_zero


pta_ctrl:
    lda ZP_KEYB_FLAGS
    and #PS2_RELEASE
    bne @pta_ctrl_rel
    lda ZP_KEYB_FLAGS
    ora #PS2_CTRL
    bra @pta_ctrl_save
@pta_ctrl_rel:
    lda ZP_KEYB_FLAGS
    and #PS2_CTRL_END
@pta_ctrl_save:
    sta ZP_KEYB_FLAGS
    jmp pta_return_zero


pta_alt:
    lda ZP_KEYB_FLAGS
    and #PS2_RELEASE
    bne @pta_alt_rel
    lda ZP_KEYB_FLAGS
    and #PS2_SPECIAL
    bne @pta_alt_altgr
    ora #PS2_ALT
    bra @pta_alt_save
@pta_alt_altgr:
    ora #PS2_ALTGR
    bra @pta_alt_save
@pta_alt_rel:
    lda ZP_KEYB_FLAGS
    and #PS2_ALT_END
@pta_alt_save:
    sta ZP_KEYB_FLAGS
    jmp pta_return_zero

;================================================================================
; ORDINARY KEY PROCESSING & CLEANUP
;================================================================================

pta_ordinary:
    pla                          ; geretteten scan code wiederholen
    and #$7f                     ; In ASCII-Bereich zwingen
    tax                          ; scan code steht jetzt in .X
    
    lda ZP_KEYB_FLAGS
    and #PS2_RELEASE
    bne @release_end
    
    lda ZP_KEYB_FLAGS
    and #PS2_ALTGR
    bne @altgr_set
    
    lda ZP_KEYB_FLAGS
    and #PS2_SHIFT
    bne @shift_set
    
    lda ZP_KEYB_FLAGS
    and #PS2_CAPSLOCK
    bne @caps_set

    lda ZP_KEYB_FLAGS
    and #PS2_CTRL
    beq @ordinary_key
    
    ; check if ascii code is between a and z => means here we have Ctrl-Keys like ^L = Form Feed = Clear Screen
    lda ps2_to_ascii_lower, x
    cmp #ASCII_LOW_A                     
    bcc @no_ctrl_char
    cmp #ASCII_LOW_Z + 1          ; because bcs (Carry Set Check) means greater or equal (>=)                    
    bcs @no_ctrl_char
    and #$1f                      ; and $1f used on $61 (a) = $01, used on $7a (z) = $1a (Dec: 26)
    bra pta_end
@no_ctrl_char:
    pha                           ; es muss was auf den Stack, da in pta_return_zero der wert wieder entfernt wird
    bra pta_return_zero           ; CTRL is pressed but its not a CTRL-Char (^A-^Z) => return 0

; TODO: bis jetzt werden keine Fkt-Tasten und weitere zurückgegeben. Hier fehlt noch das Konzept.
;       siehe auch ps2_to_ascii_* tables - die scan codes z.B. $01,$05,$07 (F9,F1,F12) werden alle als ASCII $00 zurückgegeben.

@ordinary_key:
    lda ps2_to_ascii_lower, x
    bra pta_end

@release_end:
    lda #0
    bra pta_end

@altgr_set:
    lda ps2_to_ascii_altgr, x
    bra pta_end

@shift_set:
    lda ZP_KEYB_FLAGS
    and #PS2_CAPSLOCK
    bne @shift_caps
    lda ps2_to_ascii_upper, x
    bra pta_end
@shift_caps:
    lda ps2_to_ascii_lower, x
    bra pta_end

@caps_set:
    lda ps2_to_ascii_upper, x
;	bra @pta_end

pta_end:
    pha
    lda ZP_KEYB_FLAGS
    and #PS2_SPECIAL_END          ; Setzt das Special-Flag und das Release-Flag am Ende zurück
    sta ZP_KEYB_FLAGS
    pla
    ply
    plx
    rts

pta_return_zero:
    pla                           ; geretteter scan code muss vom Stack geholt werden, da er noch dort liegt
    lda #0
    ply
    plx
    rts

;================================================================================
;   ps2_set_leds - set all leds on/off
;   ————————————————————————————————————
;   Parameters:      .A is led status byte
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_set_leds:
    phx

    sta ZP_KEYB_LEDS        ; Neuen LED-Zustand zwischenspeichern

    ; 1. Befehl $ED (Set LEDs) senden
    lda #$ed
    jsr ps2_write

    ; 2. Warten, bis die Tastatur das Befehls-ACK ($FA) geschickt hat
    ldx #50
    jsr PS2_get_wait_timeout     ; Nutzen wir aus der Init-Logik!
    bcs @led_err                  ; Timeout? Dann abbrechen.
    cmp #$FA
    bne @led_err                  ; Falsche Antwort? Abbrechen.

    ; 3. Das LED-Datenbyte hinterhersenden
    lda ZP_KEYB_LEDS
    jsr ps2_write

    ; 4. Optional: Auch das zweite ACK der Tastatur abwarten und verwerfen,
    ; damit es später nicht als "Geister-Taste" im KEYB_BUFFER landet!
    ldx #50
    jsr PS2_get_wait_timeout
    
@led_err:
    plx
    rts

;================================================================================
;   ps2_set_capslock_led - set capslock led on/off
;   ————————————————————————————————————
;   Parameters:      .A is 1 => led on, 0 => led off
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ps2_set_capslock_led:
    cmp #0
    beq pcl_off
    lda ZP_KEYB_LEDS
    ora #PS2_CAPSLOCK_LED_ON
    bra pcl_do
pcl_off:
    lda ZP_KEYB_LEDS
    and #PS2_CAPSLOCK_LED_OFF
pcl_do:
    jsr ps2_set_leds
    rts

;================================================================================
;   KEYB_ihandler - PS/2 keyboard IRQ Handler
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
KEYB_ihandler:
    ; Check for VIA interrupts
;    bit KEYB_IFR
;    bmi @irq_via
;    ply                       ; restore y
;    plx                       ; restore x
;    pla                       ; restore Akku
;    rti

;@irq_via:
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

    KEYB_PREPARE_READ_CHARACTER

    lda ZP_KEYB_RD_RESULT
    KEYB_ADD_TO_BUFFER

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
    jsr __kernel_sleep

	and #%10111111
    sta KEYB_DDR              ; release clock

    ; Prepare for the next character
    KEYB_PREPARE_READ_CHARACTER

    lda #$ff
    KEYB_ADD_TO_BUFFER

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti


.segment "RODATA"

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

; Special Case Mappings (might be more, so add some if in need):
;   Left Arrow:  scancode $6b -> Ascii $14   Esc[D
;   Right Arrow: scancode $74 -> Ascii $13   Esc[C
;   Down Arrow:  scancode $72 -> Ascii $12   Esc[B
;   Up Arrow:    scancode $75 -> Ascii $11   Esc[A
;   PgUp:        scancode $7d -> Ascii $0e
;   PgDown:      scancode $7a -> Ascii $0f
;   Home:        scancode $6c -> Ascii $02
;   End:         scancode $69 -> Ascii $03
;   Ins:         scancode $70 -> Ascii $1a
;   Del:         scancode $71 -> Ascii $18

; Following Scancode_to_Ascii tables are derived from the PS/2 scan code set 2, which is used by most modern keyboards.
; The tables map the scan codes to their corresponding ASCII values,
; taking into account the state of modifier keys such as Shift, Caps Lock, and AltGr.
; Values based on this link: https://upload.wikimedia.org/wikipedia/commons/6/66/Ps2_de_keyboard_scancode_set_2.svg,
; which shows a keyboard with ordinary Ascii representation and additional scan codes on the keys.
ps2_to_ascii_lower:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, "^", $00 ; 0 - $09 = tab
    .byte $00, $00, $00, $00, $00, "q", "1", $00, $00, $00, "z", "s", "a", "w", "2", $00 ; 1
    .byte $00, "c", "x", "d", "e", "4", "3", $00, $00, " ", "v", "f", "t", "r", "5", $00 ; 2
    .byte $00, "n", "b", "h", "g", "y", "6", $00, $00, $00, "m", "j", "u", "7", "8", $00 ; 3
    .byte $00, ",", "k", "i", "o", "0", "9", $00, $00, ".", "-", "l", $ef, "p", $e2, $00 ; 4 - $ef/$f6 = ö ($ef = lcd), $e2/$df = sharp s "ß" ($e2 = lcd)
    .byte $00, $00, $e1, $00, $f5, $07, $00, $00, $00, $00, $0D, "+", $00, "#", $00, $00 ; 5 - $e1/$e4 = ä ($e1 = lcd), $f5/$fc = ü ($f5 = lcd), $07/$B4 = acute accent "´" ($07 da lcd eigen definiert), $0D = carriage return
    .byte $00, "<", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; Shifted characters (upper case letters, symbols) are mapped in the following table.
ps2_to_ascii_upper:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $df, $00 ; 0 - $09 = tab, $df/$b0 = degree sign ($df = lcd)
    .byte $00, $00, $00, $00, $00, "Q", "!", $00, $00, $00, "Z", "S", "A", "W", $22, $00 ; 1 - $22 = double quote
    .byte $00, "C", "X", "D", "E", "$", $06, $00, $00, " ", "V", "F", "T", "R", "%", $00 ; 2 - $06/$a7 = section sign "§" ($06 da lcd eigen definiert)
    .byte $00, "N", "B", "H", "G", "Y", "&", $00, $00, $00, "M", "J", "U", "/", "(", $00 ; 3
    .byte $00, ";", "K", "I", "O", "=", ")", $00, $00, ":", "_", "L", $03, "P", "?", $00 ; 4 - $03/$d6 = Ö ($03 da lcd eigen definiert)
    .byte $00, $00, $02, $00, $04, "`", $00, $00, $00, $00, $0D, "*", $00, "'", $00, $00 ; 5 - $02/$c4 = Ä ($02 da lcd eigen definiert), $04/$dc = Ü ($04 da lcd eigen definiert), $0D = carriage return
    .byte $00, ">", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; AltGr characters (special symbols) are mapped in the following table.
ps2_to_ascii_altgr:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F   
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00 ; 0 - $09 = tab
    .byte $00, $00, $00, $00, $00, "@", $00, $00, $00, $00, $00, $00, $00, $00, $b2, $00 ; 1 - $b2 = squared sign
    .byte $00, $00, $00, $00, $05, $00, $b3, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 2 - $05/$80 = euro sign ($05 da lcd eigen definiert), $b3 = cubed sign
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $e4, $00, $00, "{", "[", $00 ; 3 - $e4/$b5 = micro sign ($e4 = lcd)
    .byte $00, $00, $00, $00, $00, "}", "]", $00, $00, $00, $00, $00, $00, $00, $01, $00 ; 4 - $01/$5C = backslash ($01 da lcd eigen definiert)
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0D, "~", $00, $00, $00, $00 ; 5 - $0D = carriage return
    .byte $00, "|", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; Jump table for special keys like shift, ctrl, alt, altGr, Caps_Lock
ps2_control_table:
    .byte $58
    .word pta_capslock       ; Ohne '@' -> globales Label
    .byte $12
    .word pta_shift          ; Ohne '@'
    .byte $59
    .word pta_shift
    .byte $14
    .word pta_ctrl
    .byte $11
    .word pta_alt
    .byte $00               
    .word pta_ordinary       ; Der sichere Ausgang für normale Tasten
