;==================================================================================================
;   Keyboard API to translate scancodes to ASCII, VKEYs and Protocol-Keys masks (shift, ctrl, alt)
;        This module needs a driver like ps2-driver.s for example to supply scancodes
;==================================================================================================

.include "cpu.inc"

.include "ascii.h"
.include "sysdata.inc"
.include "keyb_driver.inc"
.include "keyboard.h"

.export KEYB_init
.export KEYB_get
.export KEYB_peek
.export KEYB_is_shift
.export KEYB_is_ctrl
.export KEYB_is_alt
.export KEYB_is_altgr
.export KEYB_is_start
.export KEYB_is_menu
.export KEYB_is_fn
.export KEYB_is_capslock
.export KEYB_is_scroll

.export KEYB_ihandler

.macro REL_SP_END
    stz ZP_KEYB_IS_RELEASE
    stz ZP_KEYB_IS_SPECIAL
.endmacro

.segment "OS_CODE"

;==================================================================================================
;   KEYB_init - initializes the keyboard
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A is Status of init (KB_STATUS_OK or KB_STATUS_ERR)
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_init:
    jsr PS2_DRV_init

    cmp #PS2_DRV_STATUS_ERR
    beq @init_err
    lda #KB_STATUS_OK
    bra @init_exit
@init_err:
    lda #KB_STATUS_ERR
@init_exit:
    rts

;==================================================================================================
;   KEYB_get - Get ASCII from keyboard buffer, waits for next keystroke/scancode
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as ASCII or $00 if special key (ctrl, shift, ...) or
;                       no valid scancode
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_get:
@keyb_get_wait:	
    jsr PS2_DRV_pop_scancode
    bcc @keyb_get_gotchar         ; Carry Clear means got scancode from keyboard buffer

	; The buffer is empty, wait for an interrupt
	wai
	cli
	bra @keyb_get_wait

@keyb_get_gotchar:
    jsr keyb_to_ascii

	rts

;==================================================================================================
;   KEYB_peek - Get ASCII from keyboard buffer, doesn't wait for next keystroke
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as ASCII or $00 if special key (ctrl, shift, ...) or
;                       no valid scancode
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_peek:
    jsr PS2_DRV_pop_scancode
    bcs @keyb_peek_exit                ; Carry Set means no scancode available in keyboard buffer
    jsr keyb_to_ascii
@keyb_peek_exit:
    rts

;==================================================================================================
;   KEYB_is_shift - returns 0 if shift is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if shift is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_shift:
    lda ZP_KEYB_FLAGS
	and #KB_SHIFT
    rts

;==================================================================================================
;   KEYB_is_ctrl - returns 0 if ctrl is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if ctrl is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_ctrl:
    lda ZP_KEYB_FLAGS
	and #(KB_CTRL_LEFT | KB_CTRL_RIGHT)
    rts

;==================================================================================================
;   KEYB_is_ctrl_left - returns 0 if ctrl-left is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if ctrl-left is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_ctrl_left:
    lda ZP_KEYB_FLAGS
	and #KB_CTRL_LEFT
    rts

;==================================================================================================
;   KEYB_is_ctrl_right - returns 0 if ctrl-right is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if ctrl-right is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_ctrl_right:
    lda ZP_KEYB_FLAGS
	and #KB_CTRL_RIGHT
    rts

;==================================================================================================
;   KEYB_is_alt - returns 0 if alt is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if alt is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_alt:
    lda ZP_KEYB_FLAGS
	and #KB_ALT
    rts

;==================================================================================================
;   KEYB_is_altgr - returns 0 if altgr is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if altgr is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_altgr:
    lda ZP_KEYB_FLAGS
	and #KB_ALTGR
    rts

;==================================================================================================
;   KEYB_is_start - returns 0 if start is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if start is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_start:
    lda ZP_KEYB_FLAGS
	and #KB_START
    rts

;==================================================================================================
;   KEYB_is_menu - returns 0 if menu is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if menu is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_menu:
    lda ZP_KEYB_FLAGS
	and #KB_MENU
    rts

;==================================================================================================
;   KEYB_is_fn - returns 0 if fn is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if fn is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_fn:
    lda ZP_KEYB_FLAGS
	and #KB_FN
    rts

;==================================================================================================
;   KEYB_is_capslock - returns 0 if capslock is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if capslock is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_capslock:
    lda ZP_KEYB_IS_CAPSLOCK
    rts

;==================================================================================================
;   KEYB_is_scroll - returns 0 if scroll is not set, != 0 else
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A as 0 if scroll is not set, != 0 else
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_is_scroll:
    lda ZP_KEYB_IS_SCROLL
    rts

;==================================================================================================
;   keyb_to_ascii - converts PS2-scancode to ASCII
;   ——————————————————————————————————————————————————
;   Parameters:      .A is PS2-scancode
;   Returned Values: .Carry = 0: Usefull key =>
;                      .A = ASCII or $00 if modifier key (ctrl, shift, ...)
;                      .X = ???scancode or ID???
;                    .Carry = 1: Useless key like Release-, Special- or invalid scancode
;                      .A = 0
;                      .X = ???scancode or ID???
;   Destroys:        .A
;   ——————————————————————————————————————————————————
;==================================================================================================
keyb_to_ascii:
    phx
    phy
    pha                           ; save scancode to Stack

    ; Nur zum Test: print scancode as hex
    ; jsr LCD_print_hex

    ; Test code, ob bei Drücken 'A' (key code = $1c) ein 'A' ausgegeben wird.	
    ; cmp #$1c                     ; key code $1c = 'A' in ascii
    ; bne @kta_chk_special
    ; tax                          ; swap A to X => to have the index into the lookup tables
    ; lda ps2_to_ascii_upper, x
    ; pla
    ; ply
    ; plx
    ; rts

    ; Check for protocol scancodes ($E0, $F0, invalid scancode)
@kta_chk_special:
    cmp #$e0                      ; Special scancode sequences like $EO $11 = AltGr
    bne @kta_chk_release
    lda #1
    sta ZP_KEYB_IS_SPECIAL
    jmp kta_return_zero           ; JMP insteadc of BRA due to distance

@kta_chk_release:
    cmp #$f0                      ; Release scanode $F0 like $F0 $1C = user released 'A'
    bne @kta_chk_invalid
    lda #1
    sta ZP_KEYB_IS_RELEASE
    jmp kta_return_zero           ; JMP insteadc of BRA due to distance

@kta_chk_invalid:
    cmp #$80                      ; < $80 ok, else invalid scancode (Framing Error etc.)
    bcc @start_table              ; Carry clear heißt less than (<)
    pla                           
    lda #0
    sec
    jmp kta_end                   ; invalid scnacode => clear everything: Release-, Special-Flag, get saved .A from Stack and return .A=0, .CF=1

@start_table:
    ; Parse Jump Table for protocol and meta scancodes
    pla                           ; get saved .A from Stack
    ldy #0
@loop:
    cmp keyb_jump_table, y        ; is scancode in jump table
    beq @found_match
    ldx keyb_jump_table, y
    beq @found_match              ; At $00 -> fallback to ordinary keys/scancodes
    iny
    iny
    iny                           ; 3 Bytes ahead (1 Byte Scancode + 2 Bytes Target Address)
    bra @loop

@found_match:
    pha                           ; save scancode to Stack
    iny                           ; Adresses Low-Byte
    lda keyb_jump_table, y
    sta ZP_KEYB_JMP_PTR                   
    iny                           ; Adresses High-Byte
    lda keyb_jump_table, y
    sta ZP_KEYB_JMP_PTR_HI
    jmp (ZP_KEYB_JMP_PTR)         ; Indirect jump to handler routine

;==================================================================================================
; Special key/scancode handlers
;==================================================================================================

kta_scroll:
    lda ZP_KEYB_IS_RELEASE        ; Prüfe, ob es ein Loslass-Event ist (z.B. $F0 $7e)
    beq @kta_scroll_end           ; Wenn 0 (= gedrückt), ignorieren wir das Event völlig!

    ; --- SCROLL RELEASE ($F0 $7e) -> HIER TOGGELN WIR ---
    REL_SP_END                    ; Release und Special direkt wieder löschen
    lda ZP_KEYB_IS_SCROLL
    eor #1                        ; Scroll Zustand invertieren
    sta ZP_KEYB_IS_SCROLL
    jsr PS2_DRV_set_scrollock_led ; LED basierend auf neuem Zustand setzen
@kta_scroll_end:
    jmp kta_return_zero           ; Gedrückt halten/Wiederholen wird komplett ignoriert


kta_capslock:
    lda ZP_KEYB_IS_RELEASE        ; Prüfe, ob es ein Loslass-Event ist (z.B. $F0 $7e)
    beq @kta_capslock_end         ; Wenn 0 (= gedrückt), ignorieren wir das Event völlig!

    ; --- CAPS LOCK RELEASE ($F0 $58) -> HIER TOGGELN WIR ---
    REL_SP_END                    ; Release und Special direkt wieder löschen
    lda ZP_KEYB_IS_CAPSLOCK
    eor #%00000001                ; CapsLock Zustand invertieren
    sta ZP_KEYB_IS_CAPSLOCK
    jsr PS2_DRV_set_capslock_led ; LED basierend auf neuem Zustand setzen
@kta_capslock_end:
    jmp kta_return_zero           ; Gedrückt halten/Wiederholen wird komplett ignoriert


kta_shift:
    lda ZP_KEYB_IS_RELEASE        ; Prüfe, ob es ein Loslass-Event ist
    bne @kta_shift_rel
    lda #KB_SHIFT                 ; 65C02: Akkumulator mit Bitmaske laden
    tsb ZP_KEYB_FLAGS             ; Test and Set Bits (setzt das Shift-Bit)
    bra @kta_shift_save
@kta_shift_rel:
    lda #KB_SHIFT                 ; 65C02: Akkumulator mit Bitmaske laden
    trb ZP_KEYB_FLAGS             ; Test and Reset Bits (löscht das Shift-Bit garantiert!)
@kta_shift_save:
    pla
    lda #0
    sec
    jmp kta_end


kta_ctrl:
    lda ZP_KEYB_IS_SPECIAL        
    bne @kta_ctrl_right
    lda ZP_KEYB_IS_RELEASE        
    bne @kta_ctrl_rel
    lda #KB_CTRL_LEFT
    tsb ZP_KEYB_FLAGS
    bra @kta_ctrl_save
@kta_ctrl_rel:
    lda #KB_CTRL_LEFT
    trb ZP_KEYB_FLAGS
    bra @kta_ctrl_save
@kta_ctrl_right:
    lda ZP_KEYB_IS_RELEASE        
    bne @kta_ctrl_right_rel
    lda #KB_CTRL_RIGHT
    tsb ZP_KEYB_FLAGS
    bra @kta_ctrl_save
@kta_ctrl_right_rel:
    lda #KB_CTRL_RIGHT
    trb ZP_KEYB_FLAGS
@kta_ctrl_save:
    pla
    lda #0
    sec
    jmp kta_end


kta_alt:
    lda ZP_KEYB_IS_SPECIAL        
    bne @kta_alt_altgr
    lda ZP_KEYB_IS_RELEASE        
    bne @kta_alt_rel
    lda #KB_ALT
    tsb ZP_KEYB_FLAGS
    bra @kta_alt_save
@kta_alt_rel:
    lda #KB_ALT
    trb ZP_KEYB_FLAGS
    bra @kta_alt_save
@kta_alt_altgr:
    lda ZP_KEYB_IS_RELEASE        
    bne @kta_altgr_rel
    lda #KB_ALTGR
    tsb ZP_KEYB_FLAGS
    bra @kta_alt_save
@kta_altgr_rel:
    lda #KB_ALTGR
    trb ZP_KEYB_FLAGS
@kta_alt_save:
    pla
    lda #0
    sec
    jmp kta_end

;==================================================================================================
; ORDINARY KEY PROCESSING & CLEANUP
;==================================================================================================

kta_ordinary:
    pla                           ; geretteten scan code wiederholen
    and #$7f                      ; In ASCII-Bereich zwingen
    tax                           ; scan code steht jetzt in .X
    
    lda ZP_KEYB_IS_RELEASE        ; Prüfe, ob es ein Loslass-Event ist (z.B. $F0 $7e)
    bne @release_end
    
    lda ZP_KEYB_FLAGS
    and #KB_ALTGR
    bne @altgr_set
    
    lda ZP_KEYB_FLAGS
    and #KB_SHIFT
    bne @shift_set
    
    lda ZP_KEYB_IS_CAPSLOCK
    bne @caps_set

    lda ZP_KEYB_FLAGS
    and #(KB_CTRL_LEFT | KB_CTRL_RIGHT)
    beq @ordinary_key
    
    ; CTRL-KEY: check if ascii code is between a and z => means here we have Ctrl-Keys like ^L = Form Feed = Clear Screen
    lda ps2_to_ascii_lower, x
    cmp #ASCII_LOW_A                     
    bcc @no_ctrl_char
    cmp #ASCII_LOW_Z + 1          ; because bcs (Carry Set Check) means greater or equal (>=)                    
    bcs @no_ctrl_char
    and #$1f                      ; and $1f used on $61 (a) = $01, used on $7a (z) = $1a (Dec: 26)
    clc
    bra kta_end
@no_ctrl_char:
    pha                           ; es muss was auf den Stack, da in kta_return_zero der wert wieder entfernt wird
    bra kta_return_zero           ; CTRL is pressed but its not a CTRL-Char (^A-^Z) => return 0

; TODO: bis jetzt werden keine Fkt-Tasten und weitere zurückgegeben. Hier fehlt noch das Konzept.
;       siehe auch ps2_to_ascii_* tables - die scan codes z.B. $01,$05,$07 (F9,F1,F12) werden alle als ASCII $00 zurückgegeben.
; TODO: wenn CTRL gedrückt ist, werden nur die Keys A-Z behandelt. Ein CTRL-TAB z.B. wird als $00 zurückgegeben.

@ordinary_key:
    lda ps2_to_ascii_lower, x
    clc
    bra kta_end

@release_end:
    lda #0
    sec
    bra kta_end

@altgr_set:
    lda ps2_to_ascii_altgr, x
    clc
    bra kta_end

@shift_set:
    lda ZP_KEYB_IS_CAPSLOCK
    bne @shift_caps
    lda ps2_to_ascii_upper, x
    clc
    bra kta_end
@shift_caps:
    lda ps2_to_ascii_lower, x
    clc
    bra kta_end

@caps_set:
    lda ps2_to_ascii_upper, x
    clc
;	bra @kta_end

kta_end:
    REL_SP_END                    ; Clear Special- and Release-Flag
    ply
    plx
    rts

kta_return_zero:
    pla                           ; geretteter scan code muss vom Stack geholt werden, da er noch dort liegt
    lda #0
    sec
    ply
    plx
    rts

;==================================================================================================
;   KEYB_ihandler - Keyboard IRQ Handler
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_ihandler:
    jmp PS2_DRV_ihandler


.segment "OS_DATA_RO"

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
    .byte $00, $00, $00, $00, $00, "q", "1", $00, $00, $00, "y", "s", "a", "w", "2", $00 ; 1
    .byte $00, "c", "x", "d", "e", "4", "3", $00, $00, " ", "v", "f", "t", "r", "5", $00 ; 2
    .byte $00, "n", "b", "h", "g", "z", "6", $00, $00, $00, "m", "j", "u", "7", "8", $00 ; 3
    .byte $00, ",", "k", "i", "o", "0", "9", $00, $00, ".", "-", "l", $ef, "p", $e2, $00 ; 4 - $ef/$f6 = ö ($ef = lcd), $e2/$df = sharp s "ß" ($e2 = lcd)
    .byte $00, $00, $e1, $00, $f5, $07, $00, $00, $00, $00, $0D, "+", $00, "#", $00, $00 ; 5 - $e1/$e4 = ä ($e1 = lcd), $f5/$fc = ü ($f5 = lcd), $07/$B4 = acute accent "´" ($07 lcd self defined), $0D = carriage return
    .byte $00, "<", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; Shifted characters (upper case letters, symbols) are mapped in the following table.
ps2_to_ascii_upper:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $df, $00 ; 0 - $09 = tab, $df/$b0 = degree sign ($df = lcd)
    .byte $00, $00, $00, $00, $00, "Q", "!", $00, $00, $00, "Y", "S", "A", "W", $22, $00 ; 1 - $22 = double quote
    .byte $00, "C", "X", "D", "E", "$", $a7, $00, $00, " ", "V", "F", "T", "R", "%", $00 ; 2 - $a7 = section sign "§"
    .byte $00, "N", "B", "H", "G", "Z", "&", $00, $00, $00, "M", "J", "U", "/", "(", $00 ; 3
    .byte $00, ";", "K", "I", "O", "=", ")", $00, $00, ":", "_", "L", $03, "P", "?", $00 ; 4 - $03/$d6 = Ö ($03 lcd self defined)
    .byte $00, $00, $02, $00, $04, "`", $00, $00, $00, $00, $0D, "*", $00, "'", $00, $00 ; 5 - $02/$c4 = Ä ($02 lcd self defined), $04/$dc = Ü ($04 lcd self defined), $0D = carriage return
    .byte $00, ">", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; AltGr characters (special symbols) are mapped in the following table.
ps2_to_ascii_altgr:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F   
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00 ; 0 - $09 = tab
    .byte $00, $00, $00, $00, $00, "@", $00, $00, $00, $00, $00, $00, $00, $00, $b2, $00 ; 1 - $b2 = squared sign
    .byte $00, $00, $00, $00, $05, $00, $b3, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 2 - $05/$80 = euro sign ($05 lcd self defined), $b3 = cubed sign
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $e4, $00, $00, "{", "[", $00 ; 3 - $e4/$b5 = micro sign ($e4 = lcd)
    .byte $00, $00, $00, $00, $00, "}", "]", $00, $00, $00, $00, $00, $00, $00, $01, $00 ; 4 - $01/$5C = backslash ($01 lcd self defined)
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0D, $06, $00, $00, $00, $00 ; 5 - $0D = carriage return, $06/"~" Tilde ($06 lcd self defined) 
    .byte $00, "|", $00, $00, $00, $00, $08, $00, $00, $03, $00, $14, $02, $00, $00, $00 ; 6 - $08 = backspace, $03 = end, $14 = left, $02 = home
    .byte $1a, $18, $12, $00, $13, $11, $1B, $00, $00, $00, $0f, $00, $00, $0e, $00, $00 ; 7 - $1a = ins, $18 = del, $12 = down, $13 = right, $11 = up, $1B = esc, $0f = PgDown, $0e = PgUp

; Jump table for special keys like shift, ctrl, alt, altGr, Caps_Lock
keyb_jump_table:
    .byte $58
    .word kta_capslock
    .byte $12
    .word kta_shift
    .byte $59
    .word kta_shift
    .byte $14
    .word kta_ctrl
    .byte $11
    .word kta_alt
    .byte $7e
    .word kta_scroll
    .byte $00               
    .word kta_ordinary       ; Der sichere Ausgang für normale Tasten
