.ifndef _KEYBOARD_H_
_KEYBOARD_H_ = 1

; Sepcial keys flags
KB_RELEASE          = %00000001        ; key was released, e.g. shift was pressed and now it is released again
KB_CAPSLOCK         = %00000010
KB_SHIFT            = %00000100
KB_CTRL             = %00001000
KB_ALTGR            = %00010000
KB_ALT              = %00100000
KB_FN               = %01000000
KB_SPECIAL          = %10000000        ; $e0 to detect key sequences like $e0 $11 = AltGr key

; End Sepcial keys flags (used as e.g. "and KB_SHIFT_END" to unset the SHIFT Flag)
KB_RELEASE_END      = %01111110
KB_CAPSLOCK_END     = %01111100
KB_SHIFT_END        = %01111010
KB_CTRL_END         = %01110110
KB_ALTGR_END        = %01101110
KB_ALT_END          = %01011110
KB_FN_END           = %00111110
KB_SPECIAL_END      = %01111110

KB_STATUS_OK        = $00
KB_STATUS_ERR       = $01

.endif
