.ifndef _KEYBOARD_H_
_KEYBOARD_H_ = 1

; Meta keys flags
KB_SHIFT            = %00000001
KB_CTRL             = %00000010
KB_ALT              = %00000100
KB_ALTGR            = %00001000
KB_START            = %00010000
KB_MENU             = %00100000
KB_FN               = %01000000

; Keyboard status
KB_STATUS_OK        = $00
KB_STATUS_ERR       = $01

.endif
