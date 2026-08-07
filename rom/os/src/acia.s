.include "constants.inc"
.include "sysram.inc"
.include "kernelUtils.inc"
.include "acia.h"

.export ACIA_init
.export ACIA_get_byte
.export ACIA_send_byte
.export ACIA_send_string

.export ACIA_ihandler

.macro wr_acia_buf     ; write into ACIA buf and handle pointer. Put value in increment pointer.
    ldx ZP_ACIA_WR_PTR
    sta ACIA_BUFFER, X
    inx
    cpx #ACIA_BUFFER_SIZE
    bne @wr_acia_buf_end
    ldx #0
@wr_acia_buf_end:
    stx ZP_ACIA_WR_PTR
.endmacro

.macro rd_acia_buf     ; read from ACIA buf and handle pointer. Read value and increment pointer.
    ldx ZP_ACIA_RD_PTR
    lda ACIA_BUFFER, X
    inx
    cpx #ACIA_BUFFER_SIZE
    bne @rd_acia_buf_end
    ldx #0
@rd_acia_buf_end:
    stx ZP_ACIA_RD_PTR
.endmacro

.macro acia_buf_dif    ; Subtract the buffer pointers (wrap around is fine)
    lda ZP_ACIA_WR_PTR
    sec
    sbc ZP_ACIA_RD_PTR
.endmacro

.segment "CODE"

;================================================================================
;
;   ACIA_init - initializes the R6551 // RS232 Serial communications
;
;   ————————————————————————————————————
;   Parameters:      none
;
;   Returned Values: none
;
;   Destroys:        none
;   ————————————————————————————————————
;
;================================================================================
ACIA_init:
    pha	
	
    stz ZP_ACIA_WR_PTR             ; initialize ACIA write pointer
    stz ZP_ACIA_RD_PTR             ; initialize ACIA read pointer

    lda #(ACIA_HARDWARE_RESET)
    sta ACIA_STATUS
    lda #(ACIA_PARITY_DISABLE | ACIA_ECHO_DISABLE | ACIA_TX_INT_DISABLE_RTS_LOW | ACIA_RX_INT_ENABLE | ACIA_DTR_LOW)
    sta ACIA_COMMAND
    lda #(ACIA_STOP_BITS_1 | ACIA_DATA_BITS_8 | ACIA_CLOCK_INT | ACIA_BAUD_19200)
    sta ACIA_CONTROL
	
    pla
	rts

;================================================================================
;
;   ACIA_get_byte - Return one byte from RX buffer in .A
;
;   ————————————————————————————————————
;   Parameters:      none
;
;   Returned Values: .A, C flag (set if data exist, cleared if no data)
;
;   Destroys:        .A
;   ————————————————————————————————————
;
;================================================================================
ACIA_get_byte:
    sei
    ;;  Check to see if there is a character.
    acia_buf_dif
    beq @ACIA_no_char_available
    phx                         ; Reading from buffer messes with X.
    rd_acia_buf                 ; Get the character.
    plx
    ;; jsr ACIA_send_byte          ; Echo
    sec                         ; Indicate it is valid.
    cli
    rts
@ACIA_no_char_available:
    clc                         ; Indicate no char available.
    cli
    rts

;================================================================================
;
;   ACIA_send_byte - Send one byte to TX buffer
;
;   ————————————————————————————————————
;   Parameters:      .A byte to send
;
;   Returned Values: none
;
;   Destroys:        none
;   ————————————————————————————————————
;
;================================================================================
ACIA_send_byte:
;    sei
;    pha                        ; save A
    phx
	phy
@ACIA_wait_txd_empty:
;    lda ACIA_STATUS            ; Read ACIA status register
;    and #$10
;    beq @ACIA_wait_txd_empty
;    pla                        ; ELSE, restore ACCUMULATOR from STACK
    sta ACIA_DATA              ; Send the byte.
;    jsr ACIA_delay             ; Required delay - Comment out for working 6551/65C51!
    ldy #0	
    ldx #6
	jsr __kernel_sleep         ; wait 600us (more than 520us for 19200 baud)
;    cli
    ply
	plx
    rts                       

;================================================================================
;
;   ACIA_send_string - Send null-terminated string
;
;   ————————————————————————————————————
;   Parameters:      ZP_ACIA_SPTR, ZP_ACIA_SPTR+1 string pointer
;
;   Returned Values: none
;
;   Destroys:        none
;   ————————————————————————————————————
;
;================================================================================
ACIA_send_string:
    pha
    phx
    phy
    ldy #$00
@string_loop:
    lda (ZP_ACIA_SPTR),y
    beq @end_loop
    jsr ACIA_send_byte
    iny
    bne @string_loop
    inc ZP_ACIA_SPTR+1       ; we are crossing page
    bra @string_loop
@end_loop:
    ply
    plx
    pla
    rts

;================================================================================
;
;   ACIA_ihandler - ACIA IRQ Handler
;
;   ————————————————————————————————————
;   Parameters:      none
;
;   Returned Values: none
;
;   Destroys:        none
;   ————————————————————————————————————
;
;================================================================================
ACIA_ihandler:                        ; IRQ handler for ACIA RX. Must be called by overall IRQ handler
    lda ACIA_STATUS
;    and #$07                ; Check for errors.
;    bne @ACIA_ihandler_end  ; Ignore errors.
    and #$08                 ; Check for RX byte available
    beq @ACIA_ihandler_end   ; No byte available.

    ; There is a byte to get.
    lda ACIA_DATA
    wr_acia_buf

    ; Check how many bytes in the buffer are used.
;    acia_buf_dif
;    cmp #$F0
;    bcc @ACIA_ihandler_end
    ; There are only 15 chars left - de-assert RTS
;    lda #$01
;    sta ACIA_COMMAND

@ACIA_ihandler_end:
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti
