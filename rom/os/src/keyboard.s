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

.export KEYB_is_numlock
.export KEYB_is_capslock
.export KEYB_is_scroll

.export KEYB_ihandler

; --- Modifikator-Flags für den X-Register-Rückgabewert ---
X_FLAG_SHIFT = %01000000
X_FLAG_CTRL  = %10000000

.macro REL_SP_END
    stz ZP_KEYB_IS_RELEASE
    stz ZP_KEYB_IS_SPECIAL
.endmacro

.segment "OS_CODE"

;==================================================================================================
;   KEYB_init - initializes the keyboard system
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

    ; Initiale Standard-Tabelle für den ASCII-Pointer setzen
    jsr keyb_updateActiveTable

    lda #KB_STATUS_OK
    bra @init_exit
@init_err:
    lda #KB_STATUS_ERR
@init_exit:
    rts

;==================================================================================================
;   KEYB_get - Get ASCII or VirtID from buffer, waits for next keystroke/scancode (blocking)
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .Carry = 0: Valid key event
;                      .A = ASCII char, Control Code, or $00 if pure function key
;                      .X = Virtual Key ID (for special keys) or physischer Scancode (for text)
;   Destroys:        .A, .X
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_get:
@keyb_get_wait:
    jsr PS2_DRV_pop_scancode
    bcc @keyb_get_gotchar         ; Carry Clear = got scancode

    wai                             ; don't waste energy / waiting for interrupt
    cli
    bra @keyb_get_wait

@keyb_get_gotchar:
    jsr keyb_to_ascii
    bcs KEYB_get                  ; Carry = 1 => wait
    rts                             ; return: A = ASCII, X = VK-ID/scancode, Carry = 0

;==================================================================================================
;   KEYB_peek - Checks keyboard buffer, doesn't wait for next keystroke (non-blocking)
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: .Carry = 0: Valid key event available (A and X populated like KEYB_get)
;                    .Carry = 1: Keyboard buffer is empty
;   Destroys:        .A, .X
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_peek:
    jsr PS2_DRV_pop_scancode
    bcs @keyb_peek_exit           ; buffer empty -> Return with Carry = 1
    jsr keyb_to_ascii             ; Process.
@keyb_peek_exit:
    rts

;==================================================================================================
;   KERNEL GETTERs for Permanent Toggle State Keys
;==================================================================================================
;==================================================================================================
;   KERNEL GETTERs - Convenience helpers derived from the .X packet (race-free per-event snapshot)
;   - Modifier getters take the .X packet of a specific key event, NOT the global flags.
;     They answer: "was Shift/Ctrl/Alt held at the moment of THIS event?" -> safe for multi-process.
;   - Toggle getters (numlock/capslock/scroll) are inherently GLOBAL state (LEDs, shared mode):
;     no per-event snapshot exists for them in the packet.
;==================================================================================================
KEYB_is_shift:              ; .X = key event packet; .A = 0 or KB_X_MASK_SHIFT
    txa
    and #KB_X_MASK_SHIFT
    rts

KEYB_is_ctrl:               ; .X = key event packet; .A = 0 or KB_X_MASK_CTRL
    txa
    and #KB_X_MASK_CTRL
    rts

KEYB_is_alt:                ; .X = key event packet; .A = 0 or KB_X_MASK_ALT
    txa
    and #KB_X_MASK_ALT
    rts

KEYB_is_numlock:
    lda ZP_KEYB_IS_NUMLOCK
    rts

KEYB_is_capslock:
    lda ZP_KEYB_IS_CAPSLOCK
    rts

KEYB_is_scroll:
    lda ZP_KEYB_IS_SCROLL
    rts

; Note: modifier getters destroy .A and .X (txa); toggle getters destroy .A only.

;==================================================================================================
;   keyb_updateActiveTable - Directs the high-performance ASCII table switching
;   ——————————————————————————————————————————————————
;   Parameters:      none (reads ZP_KEYB_FLAGS)
;   Returned Values: none (updates ZP_KEYB_ASCII_TABLE_PTR)
;   Destroys:        none
;   ——————————————————————————————————————————————————
;==================================================================================================
keyb_updateActiveTable:
    pha

    ; --- 1. AltGr always wins and overrides Shift/Caps ---
    lda ZP_KEYB_FLAGS
    and #KB_ALTGR
    bne @altgr_table

    ; --- 2. Evaluate CapsLock for Shift Inversion ---
    lda ZP_KEYB_IS_CAPSLOCK
    bne @capslock_active          ; If CapsLock is active, jump to inverted logic

    ; --- 3. Standard Logic (CapsLock is OFF) ---
    lda ZP_KEYB_FLAGS
    and #KB_SHIFT
    bne @shift_table              ; Shift pressed -> Upper table
@lower_table:
    lda #<ps2_to_ascii_lower
    sta ZP_KEYB_ASCII_TABLE_PTR
    lda #>ps2_to_ascii_lower
    sta ZP_KEYB_ASCII_TABLE_PTR_HI
    bra @kuat_exit

@shift_table:
    lda #<ps2_to_ascii_upper
    sta ZP_KEYB_ASCII_TABLE_PTR
    lda #>ps2_to_ascii_upper
    sta ZP_KEYB_ASCII_TABLE_PTR_HI
    bra @kuat_exit

@altgr_table:
    lda #<ps2_to_ascii_altgr
    sta ZP_KEYB_ASCII_TABLE_PTR
    lda #>ps2_to_ascii_altgr
    sta ZP_KEYB_ASCII_TABLE_PTR_HI
    bra @kuat_exit

; --- 4. Inverted Logic (CapsLock is ON) ---
@capslock_active:
    lda ZP_KEYB_FLAGS
    and #KB_SHIFT
    bne @lower_table              ; Caps + Shift -> Invert back to Lower table
    bra @shift_table              ; Caps only -> Upper table

@kuat_exit:
    pla
    rts

;==================================================================================================
;   keyb_to_ascii - converts PS2-scancode to ASCII, Control-Char or Mod/ID-Packet (O(1))
;   ————————————————————————————————————————————————————————————————————————————————————————————
;   Parameters:      .A is physical PS2-scancode
;   Returned Values: .Carry = 0: Useful key =>
;                      .A = ASCII-Char, Control-Code, or $00 if pure function key
;                      .X = Virtual Key ID (special keys) or physical scancode (text)
;                    .Carry = 1: Event swallowed (Release-, Modifier-, or invalid scancode)
;                      .A = $00
;                      .X = Irrelevant / Destroyed
;   Destroys:        .A, .X
;==================================================================================================
keyb_to_ascii:
    phy                           ; Save Y since it is used as a local working index
    pha                           ; Temporarily push scancode to protect it for parsing

    ; --- 1. Catch protocol and prefix bytes ---
    cmp #$e0
    bne @chk_release
    lda #1
    sta ZP_KEYB_IS_SPECIAL        ; Set extended sequence flag
    jmp kta_return_protocol       ; e0/f0 arrive BEFORE @process_valid popped the scancode

@chk_release:
    cmp #$f0
    bne @chk_invalid
    lda #1
    sta ZP_KEYB_IS_RELEASE        ; Set key release flag
    jmp kta_return_protocol

@chk_invalid:
    cmp #$84                      ; Valid scancodes are $00..$83 (Function key F7 has scancode $83)
    bcc kta_process_valid         ; A < $84 -> valid, continue normally
    pla                           ; Clean up stack (A >= $84 = invalid scancode)
    lda #0
    sec                           ; Carry = 1 (Invalid scancode error)
    ply                           ; Restore Y
    rts

; Protocol/prefix bytes ($E0/$F0) reach us with the scancode pha STILL on the stack
; (it was pushed at entry and not yet popped by @process_valid) -> pop it here to
; keep the [phy, pha] push/pop strictly balanced.
kta_return_protocol:
    pla                           ; Drop the scancode byte
    lda #0
    sec                           ; Carry = 1 (swallow/release event)
    ply                           ; Restore caller's Y
    rts

kta_process_valid:
    pla                           ; Pull scancode back from stack
    tay                           ; Move to Y for indirect lookup indexing

    ; --- 2. Check for modifier/meta keys (Shift, Ctrl, Alt...) ---
    ldx #0
@loop_meta:
    cmp keyb_meta_table, x
    beq @found_meta
    inx
    inx
    inx                           ; Advance to next entry (3 bytes per entry)
    cpx #KEYB_META_TABLE_SIZE
    bne @loop_meta

    ; --- 3. NumLock / Keypad matrix routing ---
    ; All numeric keypad scancodes are physically mapped between $69 and $7D
    cmp #$69
    bcc @not_in_keypad            ; Below $69? Not a keypad key
    cmp #$7E
    bcs @not_in_keypad            ; Above $7D? Not a keypad key

    ; We are hitting the keypad area! Check if NumLock is OFF
    lda ZP_KEYB_IS_NUMLOCK        ; 0 = OFF, 1 = ON
    bne @not_in_keypad            ; If NumLock is ON, process normally as ASCII text

    ; IMPORTANT: NumLock is OFF! Force execution into the function key path
    tya                           ; Restore scancode to A
    jmp kta_is_pure_special       ; Bypass ASCII tables and jump directly to SpecialTable

@not_in_keypad:
    tya                           ; Restore scancode to A for ordinary processing
    jmp kta_ordinary

@found_meta:
    inx
    lda keyb_meta_table, x
    sta ZP_KEYB_JMP_PTR
    inx
    lda keyb_meta_table, x
    sta ZP_KEYB_JMP_PTR_HI
    tya                           ; Restore scancode to A
    jmp (ZP_KEYB_JMP_PTR)         ; Indirect jump to the respective modifier handler

;==================================================================================================
; MODIFIER HANDLERS (Update ZP_KEYB_FLAGS and control hardware LEDs)
;==================================================================================================
kta_scroll:
    lda ZP_KEYB_IS_RELEASE
    bne @scroll_pressed
    jmp kta_return_useless        ; Ignore press event (trigger on release)
@scroll_pressed:
    lda ZP_KEYB_IS_SCROLL
    eor #1
    sta ZP_KEYB_IS_SCROLL         ; Toggle state flag
    jsr PS2_DRV_set_scrollock_led
    jmp kta_return_useless

kta_capslock:
    lda ZP_KEYB_IS_RELEASE
    bne @caps_pressed
    jmp kta_return_useless        ; Ignore press event (trigger on release)
@caps_pressed:
    lda ZP_KEYB_IS_CAPSLOCK
    eor #1
    sta ZP_KEYB_IS_CAPSLOCK       ; Toggle state flag
    jsr PS2_DRV_set_capslock_led
    jsr keyb_updateActiveTable   ; Update active table pointer for Shift Inversion
    jmp kta_return_useless

kta_numlock:
    lda ZP_KEYB_IS_RELEASE
    bne @num_pressed
    jmp kta_return_useless        ; Ignore press event (trigger on release)
@num_pressed:
    lda ZP_KEYB_IS_NUMLOCK
    eor #1
    sta ZP_KEYB_IS_NUMLOCK        ; Toggle state flag
    jsr PS2_DRV_set_numlock_led
    jmp kta_return_useless

kta_shift:
    lda ZP_KEYB_IS_RELEASE
    bne @rel
    lda #KB_SHIFT
    tsb ZP_KEYB_FLAGS             ; Test and Set Bit
    bra @sync
@rel:
    lda #KB_SHIFT
    trb ZP_KEYB_FLAGS             ; Test and Reset Bit
@sync:
    jsr keyb_updateActiveTable    ; Re-align ASCII table pointer immediately
    jmp kta_return_useless

kta_ctrl:
    lda ZP_KEYB_IS_SPECIAL
    bne @right
    lda ZP_KEYB_IS_RELEASE
    bne @l_rel
    lda #KB_CTRL_LEFT
    tsb ZP_KEYB_FLAGS
    bra kta_return_useless
@l_rel:
    lda #KB_CTRL_LEFT
    trb ZP_KEYB_FLAGS
    bra kta_return_useless
@right:
    lda ZP_KEYB_IS_RELEASE
    bne @r_rel
    lda #KB_CTRL_RIGHT
    tsb ZP_KEYB_FLAGS
    bra kta_return_useless
@r_rel:
    lda #KB_CTRL_RIGHT
    trb ZP_KEYB_FLAGS
    bra kta_return_useless

kta_alt:
    lda ZP_KEYB_IS_SPECIAL
    bne @altgr
    lda ZP_KEYB_IS_RELEASE
    bne @a_rel
    lda #KB_ALT
    tsb ZP_KEYB_FLAGS
    bra kta_return_useless
@a_rel:
    lda #KB_ALT
    trb ZP_KEYB_FLAGS
    bra kta_return_useless
@altgr:
    lda ZP_KEYB_IS_RELEASE
    bne @ag_rel
    lda #KB_ALTGR
    tsb ZP_KEYB_FLAGS
    bra @ag_sync
@ag_rel:
    lda #KB_ALTGR
    trb ZP_KEYB_FLAGS
@ag_sync:
    jsr keyb_updateActiveTable   ; AltGr updates active table pointer too

; Local shared discard/cleanup exit. Placed right after the modifier handlers so that
; the short branches (beq/bra) from every handler above stay within the +-128 byte range.
kta_return_useless:
    lda #0
    sec                           ; Carry = 1 (Swallow/Discard event signal)

kta_end:
    REL_SP_END                    ; Clear protocol internal states
    ply                           ; Perfectly restore original application Y register
    rts                           ; Safe exit!

;==================================================================================================
; ORDINARY KEY PROCESSING (Standard Characters & Pure Function Keys)
;==================================================================================================
kta_ordinary:
    ; Discard key release events for standard characters
    lda ZP_KEYB_IS_RELEASE
    bne kta_return_useless

    ; Fetch character directly from currently active table pointer (O(1))
    lda (ZP_KEYB_ASCII_TABLE_PTR), y
    cmp #$00
    beq kta_is_pure_special       ; If ASCII value is 0, it is a pure function key!

    ; --- PATH A: ASCII OR CONTROL-CODE CHARACTER FOUND ---
    tax                           ; Temporarily protect the ASCII value in X

    ; Check if Ctrl modifiers are active
    lda ZP_KEYB_FLAGS
    and #(KB_CTRL_LEFT | KB_CTRL_RIGHT)
    beq @return_standard_ascii    ; No Ctrl pressed -> Return regular character

    ; Ctrl active: Translate character into standard Control-Code (e.g., 'C' -> $03)
    txa
    and #$1F                      ; Masks lower 5 bits, transforming letters into control codes
    clc                           ; Clear carry (Valid key event signal)

    phy
    plx                           ; Move physical scancode from Y to X via stack
    jmp kta_end

@return_standard_ascii:
    txa                           ; Move ASCII character back to A
    clc                           ; Clear carry (Valid key event signal)

    phy
    plx                           ; Move physical scancode from Y to X via stack
    jmp kta_end

    ; --- PATH B: PURE FUNCTION KEY (A = $00, X = Flags + ID) ---
kta_is_pure_special:
    lda ZP_KEYB_IS_SPECIAL
    bne @fetch_extended
    lda StandardSpecialTable, y   ; Get Virtual ID from standard function key map
    bra @process_special_flags
@fetch_extended:
    lda ExtendedSpecialTable, y   ; Get Virtual ID from extended function key map

@process_special_flags:
    cmp #KB_VKEY_NONE
    beq kta_return_useless        ; Drop undefined/unmapped function keys completely

    ; --- SPECIAL SUB-CASE 1: KEYPAD DIVIDE (/) ---
    cmp #KB_VKEY_KP_DIV
    bne @chk_kp_enter
    lda #$2F                      ; Force ASCII literal for '/'
    clc                           ; Valid key event
    phy
    plx                           ; Pass physical scancode into X
    jmp kta_end

@chk_kp_enter:
    ; --- SPECIAL SUB-CASE 2: KEYPAD ENTER ---
    cmp #KB_VKEY_KP_ENTER
    bne @not_ascii_special
    lda #$0D                      ; Force ASCII literal for Enter (Carriage Return)
    clc                           ; Valid key event
    phy
    plx                           ; Pass physical scancode into X
    jmp kta_end

@not_ascii_special:
    ; --- REGULAR FUNCTION KEY ASSEMBLY ---
    ; At this stage, .A contains the clean Virtual ID (0-31).
    ; We push it to the stack to keep the Accumulator free for flag operations.
    pha                           ; Save clean Virtual ID on the stack

    ldx #$00                      ; Reset our flag accumulator register (.X = 0)

    ; 1. Check Shift flag directly in RAM (Bit 0 of ZP_KEYB_FLAGS)
    ;    (BBR0 is 65C02-only and not available in 65816 mode -> use and/beq)
    lda ZP_KEYB_FLAGS
    and #KB_SHIFT
    beq @chk_ctrl_spec
    ldx #KB_X_MASK_SHIFT          ; Set Shift mask bit ($40) in .X if active

@chk_ctrl_spec:
    ; 2. Check Ctrl flags (L-Ctrl bit 1, R-Ctrl bit 3)
    lda ZP_KEYB_FLAGS
    and #%00001010                ; Mask out both Ctrl bits in RAM
    beq @chk_alt_spec             ; No Ctrl active -> Skip to Alt check

    txa
    ora #KB_X_MASK_CTRL           ; Add Ctrl mask bit ($80)
    tax                           ; Save back to flag accumulator

@chk_alt_spec:
    ; 3. Check Alt flag directly in RAM (Bit 3 of ZP_KEYB_FLAGS)
    ;    (BBR3 is 65C02-only and not available in 65816 mode -> use and/beq)
    lda ZP_KEYB_FLAGS
    and #KB_ALT
    beq @combine_final
    txa
    ora #KB_X_MASK_ALT            ; Add Alt mask bit ($20)
    tax                           ; Save back to flag accumulator

@combine_final:
    ; 4. Merge Virtual ID and assembled modifier flags
    ; At this point, .X contains ONLY the flags in the upper 3 bits ($E0).
    ; The lower 5 bits of .X are guaranteed to be 0!

    pla                           ; Pull the clean Virtual ID (0-31) from stack into .A
    sta ZP_KEYB_TMP               ; Backup ID in the driver scratchpad

    txa                           ; Move all assembled flags from .X into .A
    ora ZP_KEYB_TMP               ; Merge Flags (Bits 5-7) with Virtual ID (Bits 0-4)
    tax                           ; Move the final, unified packet back into .X for the app

    lda #$00                      ; .A = $00 (Signal for the app: Pure Function Key!)
    clc                           ; Valid key event
    jmp kta_end

;==================================================================================================
;   KEYB_ihandler - Needs to be called from main iHandler routine
;   ——————————————————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ——————————————————————————————————————————————————
;==================================================================================================
KEYB_ihandler:
    jmp PS2_DRV_ihandler


;==================================================================================================
;   RODATA SELECTION - AUTOMATED LOOKUP TABLES (CA65 Safe)
;==================================================================================================

.segment "OS_DATA_RO"

;==================================================================================================
;   DATEN-TABELLEN (Hocheffizient & CA65-konform aufgebaut)
;==================================================================================================

; =================================================================================================
; 1. DIE REINEN SONDERTASTEN-MAPPINGS (Liefern A=\$00 und X=ID + KEYB-Flags)
; =================================================================================================

; --- ASCII Transliteration Interfaces (NumLock ON / Characters) ---
.align 256
ps2_to_ascii_lower:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, "^", $00 ; 0 - $09 = tab
    .byte $00, $00, $00, $00, $00, "q", "1", $00, $00, $00, "y", "s", "a", "w", "2", $00 ; 1
    .byte $00, "c", "x", "d", "e", "4", "3", $00, $00, " ", "v", "f", "t", "r", "5", $00 ; 2
    .byte $00, "n", "b", "h", "g", "z", "6", $00, $00, $00, "m", "j", "u", "7", "8", $00 ; 3
    .byte $00, ",", "k", "i", "o", "0", "9", $00, $00, ".", "-", "l", $f6, "p", $df, $00 ; 4 - $f6 = ö, $df = sharp s "ß"
    .byte $00, $00, $e4, $00, $fc, $b4, $00, $00, $00, $00, $0D, "+", $00, "#", $00, $00 ; 5 - $e4 = ä, $fc = ü, $b4 = acute accent "´", $0D = carriage return
    .byte $00, "<", $00, $00, $00, $00, $08, $00, $00, "1", $00, "4", "7", $00, $00, $00 ; 6 - $08 = backspace
    .byte "0", ",", "2", "5", "6", "8", $1B, $00, $00, "+", "3", "-", "*", "9", $00, $00 ; 7 - $1B = esc

ps2_to_ascii_upper:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $b0, $00 ; 0 - $09 = tab, $b0 = degree sign °
    .byte $00, $00, $00, $00, $00, "Q", "!", $00, $00, $00, "Y", "S", "A", "W", $22, $00 ; 1 - $22 = double quote
    .byte $00, "C", "X", "D", "E", "$", $a7, $00, $00, " ", "V", "F", "T", "R", "%", $00 ; 2 - $a7 = section sign "§"
    .byte $00, "N", "B", "H", "G", "Z", "&", $00, $00, $00, "M", "J", "U", "/", "(", $00 ; 3
    .byte $00, ";", "K", "I", "O", "=", ")", $00, $00, ":", "_", "L", $d6, "P", "?", $00 ; 4 - $d6 = Ö
    .byte $00, $00, $c4, $00, $dc, "`", $00, $00, $00, $00, $0D, "*", $00, "'", $00, $00 ; 5 - $c4 = Ä, $dc = Ü, $0D = carriage return
    .byte $00, ">", $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 6
    .byte $00, $00, $00, $00, $00, $00, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 7

ps2_to_ascii_altgr:
    ;      0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $09, $00, $00 ; 0 - $09 = tab
    .byte $00, $00, $00, $00, $00, "@", $00, $00, $00, $00, $00, $00, $00, $00, $b2, $00 ; 1 - $b2 = squared sign
    .byte $00, $00, $00, $00, $80, $00, $b3, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 2 - $80 = euro sign €, $b3 = cubed sign
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $b5, $00, $00, "{", "[", $00 ; 3 - $b5 = micro sign µ
    .byte $00, $00, $00, $00, $00, "}", "]", $00, $00, $00, $00, $00, $00, $00, $5c, $00 ; 4 - $5C = backslash \
    .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0D, "~", $00, $00, $00, $00 ; 5 - $0D = carriage return
    .byte $00, "|", $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 6
    .byte $00, $00, $00, $00, $00, $00, $1B, $00, $00, $00, $00, $00, $00, $00, $00, $00 ; 7

; --- Table A: Extended Function Keys (Prefix $E0 active) ---
ExtendedSpecialTable:
    .res $4A, KB_VKEY_NONE           ; Fill up to $49
    .byte KB_VKEY_KP_DIV             ; $4A = Keypad / (Extended)
    .res $5A - $4A - 1, KB_VKEY_NONE ; Fill up to $59
    .byte KB_VKEY_KP_ENTER           ; $5A = Keypad Enter (Extended)
    .res $69 - $5A - 1, KB_VKEY_NONE ; Fill up to $68
    .byte KB_VKEY_END                ; $69 = Keypad 1 (End)
    .res $6C - $69 - 1, KB_VKEY_NONE
    .byte KB_VKEY_HOME               ; $6C = Home
    .res $70 - $6C - 1, KB_VKEY_NONE
    .byte KB_VKEY_INS                ; $70 = Ins
    .byte KB_VKEY_DEL                ; $71 = Del
    .byte KB_VKEY_DOWN               ; $72 = Down Arrow
    .res $74 - $72 - 1, KB_VKEY_NONE
    .byte KB_VKEY_RIGHT              ; $74 = Right Arrow
    .byte KB_VKEY_UP                 ; $75 = Up Arrow
    .res $7A - $75 - 1, KB_VKEY_NONE
    .byte KB_VKEY_PGDN               ; $7A = PgDn
    .res $7D - $7A - 1, KB_VKEY_NONE
    .byte KB_VKEY_PGUP               ; $7D = PgUp
    .res 256 - $7D - 1, KB_VKEY_NONE

; --- Table B: Standard Function Keys (No prefix $E0 active, or NumLock OFF) ---
StandardSpecialTable:
    .res $01, KB_VKEY_NONE
    .byte KB_VKEY_F9                 ; $01 = F9
    .res $03 - $01 - 1, KB_VKEY_NONE
    .byte KB_VKEY_F5                 ; $03 = F5
    .byte KB_VKEY_F3                 ; $04 = F3
    .byte KB_VKEY_F1                 ; $05 = F1
    .byte KB_VKEY_F2                 ; $06 = F2
    .byte KB_VKEY_F12                ; $07 = F12
    .res $09 - $07 - 1, KB_VKEY_NONE
    .byte KB_VKEY_F10                ; $09 = F10
    .byte KB_VKEY_F8                 ; $0A = F8
    .byte KB_VKEY_F6                 ; $0B = F6
    .byte KB_VKEY_F4                 ; $0C = F4
    .res $69 - $0C - 1, KB_VKEY_NONE ; Fill up to numeric keypad start ($69)
    .byte KB_VKEY_END                ; $69 = Keypad 1 (End)
    .byte KB_VKEY_NONE               ; $6A = Keypad 5 (No function)
    .byte KB_VKEY_LEFT               ; $6B = Keypad 4 (Left)
    .byte KB_VKEY_HOME               ; $6C = Keypad 7 (Home)
    .res $70 - $6C - 1, KB_VKEY_NONE
    .byte KB_VKEY_INS                ; $70 = Keypad 0 (Ins)
    .byte KB_VKEY_DEL                ; $71 = Keypad , (Del)
    .byte KB_VKEY_DOWN               ; $72 = Keypad 2 (Down)
    .byte KB_VKEY_NONE               ; $73 = Keypad 5 (No function)
    .byte KB_VKEY_RIGHT              ; $74 = Keypad 6 (Right)
    .byte KB_VKEY_UP                 ; $75 = Keypad 8 (Up)
    .res $78 - $75 - 1, KB_VKEY_NONE
    .byte KB_VKEY_F11                ; $78 = F11
    .res $7C - $78 - 1, KB_VKEY_NONE
    .byte KB_VKEY_PGDN               ; $7C = Keypad 3 (PgDn)
    .byte KB_VKEY_PGUP               ; $7D = Keypad 9 (PgUp)
    .res $83 - $7D - 1, KB_VKEY_NONE
    .byte KB_VKEY_F7                 ; $83 = F7
    .res 256 - $83 - 1, KB_VKEY_NONE

; Meta/Modifier routing table
keyb_meta_table:
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
    .byte $76
    .word kta_numlock

; Automatically calculate the exact byte size of the meta table
KEYB_META_TABLE_SIZE = * - keyb_meta_table
