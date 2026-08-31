.ifndef _SYSRAM_INC_
_SYSRAM_INC_ = 1

.importzp ZP_LCD_COL
.importzp ZP_LCD_ROW
.importzp ZP_LCD_STR_PTR
.importzp ZP_LCD_STR_PTR_HI
.importzp ZP_LCD_BUF_IDX
.importzp ZP_KEYB_TMP
.importzp ZP_KEYB_RD_RESULT
.importzp ZP_KEYB_WR_PTR
.importzp ZP_KEYB_RD_PTR
.importzp ZP_KEYB_INIT_RESULT
.importzp ZP_KEYB_LEDS
.importzp ZP_KEYB_FLAGS
.importzp ZP_ACIA_WR_PTR
.importzp ZP_ACIA_RD_PTR
.importzp ZP_ACIA_SPTR
.importzp ZP_VIA_TIMER_INT_CNT

.import ACIA_BUFFER
.import KEYB_BUFFER
.import LCD_BUFFER
.import shell_cmd_id
.import shell_cmd_tmp
.import shell_buffer_used
.import shell_buffer

.import osversion
.import copywrite 

.include "sysram.h"
.endif
