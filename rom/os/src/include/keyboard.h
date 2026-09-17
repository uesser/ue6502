.ifndef _KEYBOARD_H_
_KEYBOARD_H_ = 1

KEYB_DDR   = VIA_DDRB
KEYB_PORT  = VIA_PORTB
KEYB_ACR   = VIA_ACR
KEYB_PCR   = VIA_PCR
KEYB_SR    = VIA_SR
KEYB_IER   = VIA_IER
KEYB_IFR   = VIA_IFR
KEYB_T2C_L = VIA_T2C_L
KEYB_T2C_H = VIA_T2C_H

; Values to send to keyboard
PS2_RESET            = $ff
PS2_SCROLLOCK_LED_ON  = %00000001
PS2_NUMLOCK_LED_ON    = %00000010
PS2_CAPSLOCK_LED_ON   = %00000100
PS2_SCROLLOCK_LED_OFF = %11111110
PS2_NUMLOCK_LED_OFF   = %11111101
PS2_CAPSLOCK_LED_OFF  = %11111011

; Konstanten für Statusmeldungen
PS2_ACK         = $fa
PS2_BAT         = $aa
PS2_CAPS_HELD   = %00100000 ; NEU: Taste ist physikalisch unten
KB_STATUS_OK    = $00
KB_STATUS_ERR   = $01

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
