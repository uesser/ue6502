;
; LCD screen
ZP_LCD_COL  = $10            ; current LCD col
ZP_LCD_ROW  = $11            ; current LCD row
ZP_LCD_STR_PTR  = $12        ; 2 byte pointer
ZP_LCD_STR_PTR_HI = $13
ZP_LCD_BUF_IDX = $14         ; 1 byte index

;
; Keyboard
ZP_KEYB_TMP = $20            ; 1 byte memory
ZP_KEYB_RD_RESULT = $21      ; 3 byte memory
ZP_KEYB_WR_PTR = $22         ; 1 byte memory
ZP_KEYB_RD_PTR = $23         ; 1 byte memory

;
; ACIA (RS232)
ZP_ACIA_WR_PTR = $30
ZP_ACIA_RD_PTR = $31
