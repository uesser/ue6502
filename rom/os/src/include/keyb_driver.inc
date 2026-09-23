.ifndef _KEYB_DRIVER_INC_
_KEYB_DRIVER_INC_ = 1

.import PS2_DRV_init
.import PS2_DRV_pop_scancode
.import PS2_DRV_pop_scancode_timeout
.import PS2_DRV_set_capslock_led
.import PS2_DRV_set_numlock_led
.import PS2_DRV_set_scrollock_led

.import PS2_DRV_ihandler

.include "keyb_driver.h"

.endif
