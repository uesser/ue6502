ACIA_BUFFER = $0200           ; max size 128 ($80) byte
ACIA_BUFFER_SIZE = $80        ; 128 byte

KEYB_BUFFER = $0280           ; max size 128 ($80) byte
KEYB_BUFFER_SIZE = $80        ; 128 byte

LCD_BUFFER = $0300            ; max size 80 ($50) byte

shell_cmd_id = $0350
shell_cmd_tmp = $0351
shell_buffer_used = $0352
shell_buffer = $0353          ; max size 64 ($40) byte
