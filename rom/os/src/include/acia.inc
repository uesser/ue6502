.ifndef _ACIA_INC_
_ACIA_INC_ = 1

.include "cpu.inc"
.include "ascii.h"

.import ACIA_DATA
.import ACIA_STATUS
.import ACIA_COMMAND
.import ACIA_CONTROL

.import ACIA_init
.import ACIA_get_byte
.import ACIA_get_byte_timeout   
.import ACIA_send_byte
.import ACIA_send_string
.import ACIA_send_hex

.import ACIA_ihandler

.include "acia.h"

.endif
