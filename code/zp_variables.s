;
; LCD screen
LCD_COL  = $10            ; current LCD col
LCD_ROW  = $11            ; current LCD row
LCD_STR_PTR  = $12        ; 2 byte pointer
LCD_STR_PTR_HI = $13
LCD_BUF_IDX = $14         ; 1 byte index

;
; Keyboard
KEYB_TMP = $20            ; 1 byte memory
KEYB_RD_RESULT = $21      ; 3 byte memory
KEYB_RD_RESULT_2 = $22
KEYB_RD_RESULT_3 = $23
KEYB_WR_PTR = $24         ; 1 byte memory
KEYB_RD_PTR = $25         ; 1 byte memory

;
; ACIA (RS232)
ACIA_WR_PTR = $30
ACIA_RD_PTR = $31
