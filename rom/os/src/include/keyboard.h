.ifndef _KEYBOARD_H_
_KEYBOARD_H_ = 1

; --- Internal Kernel State Flags (Stored in ZP_KEYB_FLAGS) ---
; These bits live in the global ZP_KEYB_FLAGS byte and are managed by the kernel.
; Applications must NEVER read them directly; consume only the .X event packet.
KB_SHIFT             = $01        ; %00000001
KB_CTRL_LEFT         = $02        ; %00000010
KB_CTRL_RIGHT        = $04        ; %00000100
KB_ALT               = $08        ; %00001000
KB_ALTGR             = $10        ; %00010000
KB_START             = $20        ; %00100000
KB_MENU              = $40        ; %01000000
KB_FN                = $80        ; %10000000

; --- Application Modifier Bitmasks (Returned in Bits 5-7 of the .X Register) ---
; These decode the per-event snapshot; they do NOT reference the global flags.
KB_X_MASK_ALT        = $20        ; Bit 5: Alt key is held (%00100000)
KB_X_MASK_SHIFT      = $40        ; Bit 6: Any Shift key is held (%01000000)
KB_X_MASK_CTRL       = $80        ; Bit 7: Any Ctrl key is held (%10000000)
KB_X_MASK_MODIFIERS  = $E0        ; Mask to isolate all modifier flags (%11100000)
KB_X_MASK_KB_VKEY_ID = $1F        ; Mask to isolate the clean Virtual ID (%00011111)

; --- Virtual Special Key IDs (Bits 0-4 of the .X Register, Range $00-$1F) ---
KB_VKEY_UP           = $00
KB_VKEY_DOWN         = $01
KB_VKEY_LEFT         = $02
KB_VKEY_RIGHT        = $03
KB_VKEY_INS          = $04
KB_VKEY_DEL          = $05
KB_VKEY_HOME         = $06
KB_VKEY_END          = $07
KB_VKEY_PGUP         = $08
KB_VKEY_PGDN         = $09
KB_VKEY_F1           = $0A
KB_VKEY_F2           = $0B
KB_VKEY_F3           = $0C
KB_VKEY_F4           = $0D
KB_VKEY_F5           = $0E
KB_VKEY_F6           = $0F
KB_VKEY_F7           = $10
KB_VKEY_F8           = $11
KB_VKEY_F9           = $12
KB_VKEY_F10          = $13
KB_VKEY_F11          = $14
KB_VKEY_F12          = $15
KB_VKEY_KP_DIV       = $16        ; Keypad /
KB_VKEY_KP_ENTER     = $17        ; Keypad Enter
KB_VKEY_WIN          = $18        ; Windows / Start Key
KB_VKEY_MENU         = $19        ; Context Menu Key
KB_VKEY_NONE         = $FF

; --- Keyboard Initialization Status ---
KB_STATUS_OK  = $00
KB_STATUS_ERR = $01

; --- Convenience Getters (race-free): take the caller's own .X event packet ---
;   KEYB_is_shift(.X)  -> .A = 0 or KB_X_MASK_SHIFT
;   KEYB_is_ctrl(.X)   -> .A = 0 or KB_X_MASK_CTRL
;   KEYB_is_alt(.X)    -> .A = 0 or KB_X_MASK_ALT
;   These NEVER read the global ZP_KEYB_FLAGS; they decode the snapshot in .X only.

.endif
