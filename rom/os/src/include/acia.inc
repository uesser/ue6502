.ifndef _ACIA_INC_
_ACIA_INC_ = 1

.include "cpu.inc"
.include "ascii.h"

.import ACIA_init
.import ACIA_get_byte
.import ACIA_get_byte_timeout   
.import ACIA_send_byte
.import ACIA_send_string

.import ACIA_ihandler

.macro ACIA_writestr str_addr
    lda #<str_addr
    sta ACIA_SPTR
    lda #>str_addr
    sta ACIA_SPTR+1
    jsr ACIA_send_string
.endmacro

.macro ACIA_writeln str_addr
    ACIA_writestr str_addr
    jsr ACIA_send_string
	lda #ASCII_CR              ; carriage return
	jsr ACIA_send_byte
.endmacro

.include "acia.h"

.endif
