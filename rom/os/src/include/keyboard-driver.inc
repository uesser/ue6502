.ifndef _KEYBOARD_DRIVER_INC_
_KEYBOARD_DRIVER_INC_ = 1

.import KEYB_init
.import KEYB_pop_scancode
.import KEYB_pop_scancode_timeout
.import KEYB_set_capslock_led
.import KEYB_set_numlock_led
.import KEYB_set_scrollock_led

.import KEYB_ihandler

.include "keyboard-driver.h"

.endif
