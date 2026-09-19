.ifndef _KEYBOARD_DRIVER_H_
_KEYBOARD_DRIVER_H_ = 1

; Values to send to keyboard
PS2_RESET            = $ff
PS2_SCROLLOCK_LED_ON  = %00000001
PS2_NUMLOCK_LED_ON    = %00000010
PS2_CAPSLOCK_LED_ON   = %00000100
PS2_SCROLLOCK_LED_OFF = %11111110
PS2_NUMLOCK_LED_OFF   = %11111101
PS2_CAPSLOCK_LED_OFF  = %11111011

; Konstanten für Statusmeldungen
PS2_ERR         = $ff
PS2_ACK         = $fa
PS2_BAT         = $aa
PS2_SET_LEDS    = $ed

KB_STATUS_OK    = $00
KB_STATUS_ERR   = $01

.endif
