.ifndef _SYSDATA_INC_
_SYSDATA_INC_ = 1

;---------------------------------
;   Segment OS_CODE
;---------------------------------
.import SYSDATA_init

;---------------------------------
;   Segment ZEROPAGE
;---------------------------------
.importzp ZP_LCD_COL
.importzp ZP_LCD_ROW
.importzp ZP_LCD_JMP_PTR
.importzp ZP_LCD_JMP_PTR_HI
.importzp ZP_LCD_STR_PTR
.importzp ZP_LCD_STR_PTR_HI

.importzp ZP_KEYB_TMP
.importzp ZP_KEYB_RD_RESULT
.importzp ZP_KEYB_WR_PTR
.importzp ZP_KEYB_RD_PTR
.importzp ZP_KEYB_JMP_PTR
.importzp ZP_KEYB_JMP_PTR_HI
.importzp ZP_KEYB_ASCII_TABLE_PTR
.importzp ZP_KEYB_ASCII_TABLE_PTR_HI
.importzp ZP_KEYB_LEDS
.importzp ZP_KEYB_FLAGS
.importzp ZP_KEYB_IS_RELEASE
.importzp ZP_KEYB_IS_SPECIAL
.importzp ZP_KEYB_IS_NUMLOCK
.importzp ZP_KEYB_IS_CAPSLOCK
.importzp ZP_KEYB_IS_SCROLL 

.importzp ZP_ACIA_WR_PTR
.importzp ZP_ACIA_RD_PTR
.importzp ZP_ACIA_STR_PTR
.importzp ZP_ACIA_STR_PTR_HI

.importzp ZP_VIA_TIMER_INT_CNT

.importzp ZP_KERNEL_LAST_KEY
.importzp ZP_KERNEL_TAB_WIDTH

;---------------------------------
;   Segment BSS
;---------------------------------
.import ACIA_BUFFER
.import KEYB_BUFFER
.import LCD_BUFFER

.import ACIA_BUFFER_SIZE
.import KEYB_BUFFER_SIZE
.import LCD_BUFFER_SIZE

.import shell_cmd_id
.import shell_cmd_tmp
.import shell_buffer_used
.import shell_buffer

;---------------------------------
;   Segment VIDEO_DATA
;---------------------------------
.import VIDEORAM_BUFFER
.import VIDEORAM_BUFFER_SIZE

;---------------------------------
;   Segment OS_DATA_RO
;---------------------------------
.import hexmap

;---------------------------------
;   Segment OS_DATA_VERSION
;---------------------------------
.import osversion
.import copywrite

;.include "sysdata.h"
.endif
