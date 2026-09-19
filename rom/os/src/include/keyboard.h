.ifndef _KEYBOARD_H_
_KEYBOARD_H_ = 1

; Sepcial keys flags
PS2_RELEASE  = %00000001          ; key was released, e.g. shift was pressed and now it is released again
PS2_CAPSLOCK = %00000010
PS2_SHIFT    = %00000100
PS2_CTRL     = %00001000
PS2_ALTGR    = %00010000
PS2_ALT      = %00100000
PS2_FN       = %01000000
PS2_SPECIAL  = %10000000          ; $e0 to detect key sequences like $e0 $11 = AltGr key

; End Sepcial keys flags (used in e.g. and PS2_SHIFT_END)
PS2_RELEASE_END  = %01111110
PS2_CAPSLOCK_END = %01111100
PS2_SHIFT_END    = %01111010
PS2_CTRL_END     = %01110110
PS2_ALTGR_END    = %01101110
PS2_ALT_END      = %01011110
PS2_FN_END       = %00111110
PS2_SPECIAL_END  = %01111110

.endif
