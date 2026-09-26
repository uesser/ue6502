.ifndef _KEYBOARD_INC_
_KEYBOARD_INC_ = 1

; --- Core Keyboard API Function for kernel init process ---
.import KEYB_init

; --- Core Keyboard API Functions for kernel to do its own API for ordinary programs ---
.import KEYB_get
.import KEYB_peek

; --- Convenience Getter: Modifier state of a SPECIFIC event, from the caller's .X packet ---
; (race-free: answers only for the snapshot that the caller's own .X packet represents)
.import KEYB_is_shift          ; .X = event packet
.import KEYB_is_ctrl           ; .X = event packet
.import KEYB_is_alt            ; .X = event packet

; --- Permanent Toggle State Getters (inherently global: LEDs / shared keyboard mode) ---
.import KEYB_is_numlock
.import KEYB_is_capslock
.import KEYB_is_scroll

; --- Hardware Interrupt Vector ---
.import KEYB_ihandler

.include "keyboard.h"

.endif
