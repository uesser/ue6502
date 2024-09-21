LCD_DDR  = VIA_DDRA
LCD_PORT = VIA_PORTA

E  = %01000000
RW = %00100000
RS = %00010000

CPUFREQUENCY = 2 ; clock frequency in MHz for delay subroutine

lcd_col  = $0003 ; current LCD col
lcd_row  = $0004 ; current LCD row
str_ptr  = $0005 ; 2 byte pointer

lcd_buffer = $0300
lcd_buf_ind = $0007 ; 1 byte index

LCD_FIRST_LINE = 0
LCD_SECOND_LINE = 20

; 20x4
LCDROWS = 4
LCDCOLS = 20
LCDMAXCOL = 80
LCDMAXSCROLL = 60

; 16x2
;LCDROWS = 2
;LCDCOLS = 16
;LCDMAXCOL = 32
;LCDMAXSCROLL = 16

prompt: 
  .asciiz "PS/2: "

hexmap: 
  .byte "0123456789ABCDEF"

lcdrowstart:
  .byte $00 ; 20x4 and 16x2
  .byte $40 ; 20x4 and 16x2
  .byte $14 ; 20x4
  .byte $54 ; 20x4

lcdbufrowstart:
  .byte 00
  .byte 20
  .byte 40
  .byte 60

lcd_setup:
  lda #%11111111 ; Set all pins on port B to output
  sta LCD_DDR

  jsr lcd_init   ; Note: along with delay, this bludgeons A, X and Y (cold and warm reset of LCD)
  
  lda #%00101000 ; Set 4-bit mode; 2-line display; 5x8 font
  jsr lcd_instruction
  lda #%00001110 ; Display on; cursor on; blink off
  jsr lcd_instruction
  lda #%00000110 ; Increment and shift cursor; don't shift display
  jsr lcd_instruction
  lda #%00000001 ; Clear screen
  jsr lcd_instruction
  
  lda #$00
  sta lcd_buf_ind
  sta lcd_col
  sta lcd_row
  jsr lcd_setcursor
  
  ; init lcd buffer
  ldx #0
  lda #$20  ; blank/space char
@init_buf:
  sta lcd_buffer, x
  inx
  cpx #LCDMAXCOL
  bne @init_buf

;  lda #<prompt            ; Load low byte of our 16-bit value
;  sta str_ptr
;  lda #>prompt            ; Load high byte of our 16-bit value
;  sta str_ptr + 1
;  jsr print_str
  rts

lcd_wait:
  pha
  lda #%11110000  ; LCD data is input
  sta LCD_DDR
lcdbusy:
  lda #RW
  sta LCD_PORT
  lda #(RW | E)
  sta LCD_PORT
  lda LCD_PORT       ; Read high nibble
  pha             ; and put on stack since it has the busy flag
  lda #RW
  sta LCD_PORT
  lda #(RW | E)
  sta LCD_PORT
  lda LCD_PORT       ; Read low nibble
  pla             ; Get high nibble off stack
  and #%00001000
  bne lcdbusy

  lda #RW
  sta LCD_PORT
  lda #%11111111  ; LCD data is output
  sta LCD_DDR
  pla
  rts
  
delay_lcd:
  ldx #CPUFREQUENCY
@cpuloop:
  ; http://forum.6502.org/viewtopic.php?p=62581#p62581
  ; delay 9*(256*A+Y)+8 cycles
  ; assumes that the BCS does not cross a page boundary
  ; A and Y are the high and low bytes (respectively) of a 16-bit value; multiply that 16-bit value by 9, then add 8 and you get the cycle count
  ;
  pha
  phy
  
@delayloop:
  cpy #1         ; 2
  dey            ; 2
  sbc #0         ; 2
  bcs @delayloop ; 3
  
  ply
  pla
  
  dex
  bne @cpuloop
  rts

lcd_init:
  ; as per Figure 24 (page 46) of the Hitachi data sheet - yes, much like beating it with a rock!
  
  ; delay 50000 us ; > 40ms for Vcc to rise above 2.7V
  lda #21
  ldy #128
  jsr delay_lcd
  
  lda #%00000011 ; Set 4-bit mode
  sta LCD_PORT
  ora #E
  sta LCD_PORT
  and #%00001111
  sta LCD_PORT
  
  ; delay 4500 us
  lda #1
  ldy #243
  jsr delay_lcd
  
  lda #%00000011 ; Set 4-bit mode
  sta LCD_PORT
  ora #E
  sta LCD_PORT
  and #%00001111
  sta LCD_PORT
  
  ; delay 150 us
  lda #0
  ldy #16
  jsr delay_lcd
    
  lda #%00000011 ; Set 4-bit mode
  sta LCD_PORT
  ora #E
  sta LCD_PORT
  and #%00001111
  sta LCD_PORT
  
  ; This 4 bit initialization works well for cold reset (no power to the LCD) but not for resetting
  ; an already initialized and powered up LCD (without power cycling).
  ; More luck with warm reset with even number of 4 bit writes (in case LCD is already in 4 bit mode)
  
  lda #%00000010 ; Set 4-bit mode
  sta LCD_PORT
  ora #E
  sta LCD_PORT
  and #%00001111
  sta LCD_PORT
  
  rts

lcd_instruction:
  jsr lcd_wait
  pha
  lsr
  lsr
  lsr
  lsr            ; Send high 4 bits
  sta LCD_PORT
  ora #E         ; Set E bit to send instruction
  sta LCD_PORT
  eor #E         ; Clear E bit
  sta LCD_PORT
  pla
  and #%00001111 ; Send low 4 bits
  sta LCD_PORT
  ora #E         ; Set E bit to send instruction
  sta LCD_PORT
  eor #E         ; Clear E bit
  sta LCD_PORT
  rts
  
lcd_writedata:
  pha
  lsr
  lsr
  lsr
  lsr             ; Send high 4 bits
  ora #RS         ; Set RS
  sta LCD_PORT
  ora #E          ; Set E bit to send instruction
  sta LCD_PORT
  eor #E          ; Clear E bit
  sta LCD_PORT
  pla
  and #%00001111  ; Send low 4 bits
  ora #RS         ; Set RS
  sta LCD_PORT
  ora #E          ; Set E bit to send instruction
  sta LCD_PORT
  eor #E          ; Clear E bit
  sta LCD_PORT
  rts

lcd_clear:
  pha
  lda #$00
  sta lcd_col
  sta lcd_row
  lda #%00000001 ; Clear screen
  jsr lcd_instruction
  jsr lcd_setcursor
  pla
  rts
  
lcd_setcursor: ; (lcd_col, lcd_row)
  pha
  phx
  ldx lcd_row
  cpx #LCDROWS
  beq lcdskipsetcursor ; dont wrap around if (col,row) out of range (less confusion)
  
  lda lcdrowstart, x
  adc lcd_col
  ora #%10000000 ; Set DDRAM address
  jsr lcd_instruction
  
lcdskipsetcursor:
  plx
  pla
  rts
  
lcd_backspace:
  pha
  phx
  
  lda lcd_col
  beq colzero
  dec
  sta lcd_col
  bra backspaceexit
  
colzero:
  ldx lcd_row
  beq backspaceexit
  dex
  stx lcd_row
  lda #LCDCOLS
  dec
  sta lcd_col
  
backspaceexit:
  plx
  pla
  jsr lcd_setcursor
  rts
  
lcd_enter:
  phx
  
  ldx lcd_row
  inx
  cpx #LCDROWS
  bne lcd_enter_do

  jsr scroll_lcd
  jmp lcd_enter_end
  
lcd_enter_do:
  stx lcd_row
  lda lcdbufrowstart, x
  sta lcd_buf_ind
  ldx #$00
  stx lcd_col
  
lcd_enter_end:
  plx
  jsr lcd_setcursor
  rts
  
print_hex:
  phx
  pha
  
  pha
  lsr
  lsr
  lsr
  lsr
  tax
  lda hexmap, x
  jsr lcd_print_char
  pla

  and #$0F
  tax
  lda hexmap, x
  jsr lcd_print_char
  
  pla
  plx
  rts
  
print_str:
  phy
  pha
  ldy #0

print_next:
  lda (str_ptr), y
  beq print_exit
  jsr lcd_print_char
  iny
  bra print_next
print_exit:
  pla
  ply
  rts

lcd_print_char:                 ; normaler Aufruf um ein Zeichen auf LCD zu schreiben
  pha
  phx
  
  jsr lcd_write_buf
  
  plx
  pla

lcd_print_char_from_write_buf:  ; Aufruf aus lcd_write_buf raus
  pha
  phx
  
  jsr lcd_setcursor
  jsr lcd_wait
  jsr lcd_writedata
  
  ; move cursor to next cell
  inc lcd_col
  lda #LCDCOLS
  cmp lcd_col
  bne exit_print_char
  lda #0
  sta lcd_col
  inc lcd_row
  
exit_print_char:
  jsr lcd_setcursor ; to display next cell position
  
  plx
  pla
  rts

lcd_write_buf:
  phx
  pha

  ldx #LCDMAXCOL
  cpx lcd_buf_ind
  bne write_buf

  jsr scroll_lcd
  
write_buf:
  ldx lcd_buf_ind
  pla
  sta lcd_buffer, x  ; store char into lcd_buffer
  
  inc lcd_buf_ind
  
  plx
  rts

scroll_lcd:
  phx
  phy
  pha
  
  ; scroll lcd_buffer
  ldx #LCD_FIRST_LINE
  ldy #LCD_SECOND_LINE
@memcopy:
  lda lcd_buffer, y
  sta lcd_buffer, x
  inx
  iny
  cpy #LCDMAXCOL
  bne @memcopy
  ; init last line in buffer
  ldx #LCDMAXSCROLL
  lda #$20  ; blank/space char
@init_line:
  sta lcd_buffer, x
  inx
  cpx #LCDMAXCOL
  bne @init_line

  lda #LCDMAXSCROLL
  sta lcd_buf_ind
  
  ; scroll lcd
  jsr lcd_clear
  
  ldx #LCD_FIRST_LINE
@print_chars:
  lda lcd_buffer, x
  jsr lcd_print_char_from_write_buf
  inx
  cpx #LCDMAXSCROLL
  bne @print_chars
  
  pla
  ply
  plx
  rts
  