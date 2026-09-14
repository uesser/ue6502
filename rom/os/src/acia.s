.include "cpu.inc"

.include "constants.inc"
.include "sysram.inc"
.include "kernelUtils.inc"
.include "acia.h"

.export ACIA_init
.export ACIA_get_byte
.export ACIA_get_byte_timeout
.export ACIA_send_byte
.export ACIA_send_string

.export ACIA_ihandler

BAUD_RATE      = 115200  ; Bitte beachten, dass auch in init: die entspr. Baudrate eingestellt wird. Sonst passen die Berechnungen hier nicht.
BITS_PER_CHAR  = 10

; =========================================================================
; MATHE-PARSER FÜR DIE WARTESCHLEIFE (Compilezeit)
; =========================================================================
; 1. Berechne die genaue Zeit für 1 Zeichen in Mikrosekunden (aufgerundet)
;    (10 * 1.000.000) / 115200 = 86,8 -> aufgerundet 87 µs  ; Addition BAUD_RATE-1 sorgt für das Aufrunden bei der Integer-Division
TIME_US = (BITS_PER_CHAR * 1000000 + BAUD_RATE - 1) / BAUD_RATE

; 2. Berechne die Anzahl der benötigten 100µs-Blöcke (aufgerundet)
;    (87 + 99) / 100 = 1 Block (entspricht 100 µs)  ; Auch hier: + 99 sorgt für das Aufrunden bei der Integer-Division
SLEEP_BLOCKS = (TIME_US + 99) / 100

; 3. Extrahiere High- und Low-Byte für das X- und Y-Register
;    ca65 Operatoren: < zieht das Low-Byte, > zieht das High-Byte
SLEEP_LOW  = <SLEEP_BLOCKS
SLEEP_HIGH = >SLEEP_BLOCKS

; write into ACIA buf and handle pointer. Put value in increment pointer.
.macro wr_acia_buf
    ldx ZP_ACIA_WR_PTR
    sta ACIA_BUFFER, X
    inx
    cpx #ACIA_BUFFER_SIZE
    bne @wr_acia_buf_end
    ldx #0
@wr_acia_buf_end:
    stx ZP_ACIA_WR_PTR
.endmacro

; read from ACIA buf and handle pointer. Read value and increment pointer.
.macro rd_acia_buf
    ldx ZP_ACIA_RD_PTR
    lda ACIA_BUFFER, X
    inx
    cpx #ACIA_BUFFER_SIZE
    bne @rd_acia_buf_end
    ldx #0
@rd_acia_buf_end:
    stx ZP_ACIA_RD_PTR
.endmacro

; Subtract the buffer pointers to check if there is a byte to read.
.macro acia_buf_dif
    lda ZP_ACIA_WR_PTR
    sec
    sbc ZP_ACIA_RD_PTR
.endmacro

.segment "CODE"

;================================================================================
;   ACIA_init - initializes the R6551 // RS232 Serial communications
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
ACIA_init:
    pha	
	
    stz ZP_ACIA_WR_PTR             ; initialize ACIA write pointer
    stz ZP_ACIA_RD_PTR             ; initialize ACIA read pointer

    lda #(ACIA_HARDWARE_RESET)
    sta ACIA_STATUS
    lda #(ACIA_PARITY_DISABLE | ACIA_ECHO_DISABLE | ACIA_TX_INT_DISABLE_RTS_LOW | ACIA_RX_INT_ENABLE | ACIA_DTR_LOW)
    sta ACIA_COMMAND
    ; Bitte beachten, dass die hier eingestellte BAUDRATE zu Konstante BAUD_RATE (siehe weiter oben) passt.
    lda #(ACIA_DATA_BITS_8 | ACIA_STOP_BITS_1 | ACIA_CLOCK_EXT | ACIA_BAUD_16XEXT)   ; means 115.200 Baud with 1.8432 MHz clock
    sta ACIA_CONTROL
	
    pla
	rts

;================================================================================
;   ACIA_get_byte - Return one byte from RX buffer in .A
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A, C flag (set if data exist, cleared if no data)
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ACIA_get_byte:
    sei
    ;;  Check to see if there is a character.
    acia_buf_dif
    beq @ACIA_no_char_available
    phx                         ; Reading from buffer messes with X.
    rd_acia_buf                 ; Get the character.
    plx
;    jsr ACIA_send_byte          ; Echo
    sec                         ; Indicate it is valid.
    cli
    rts
@ACIA_no_char_available:
    clc                         ; Indicate no char available.
    cli
    rts

;================================================================================
;   ACIA_get_byte_timeout - Return one byte from RX buffer in .A
;                         - Waits for a byte but terminates after a 
;                           short time if nothing is received      
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: .A, C flag (set if data exist, cleared if no data)
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
ACIA_get_byte_timeout:
    phx
    phy

    ldy #$ff
@y_loop:
    ldx #$ff
@x_loop:    

    jsr ACIA_get_byte
    bcs @ACIA_got_char         ; If C flag is set, a byte was received.

    dex
    bne @x_loop
    dey
    bne @y_loop
    clc                        ; no byte received in time
    ply
    plx
    rts

@ACIA_got_char:
    ply
    plx
    rts

;================================================================================
;   ACIA_send_byte - Send one byte to TX buffer
;   ————————————————————————————————————
;   Parameters:      .A byte to send
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
ACIA_send_byte:
; Der auskommentierte code wäre der richtige, wenn der W65C51 nicht den Hardware bug hätte (googeln: w65c51 hardware bug)
;    sei
;    pha                        ; save A
    phx
	phy
;@ACIA_wait_txd_empty:
;    lda ACIA_STATUS            ; Read ACIA status register
;    and #$10
;    beq @ACIA_wait_txd_empty
;    pla                        ; ELSE, restore ACCUMULATOR from STACK
    sta ACIA_DATA              ; Send the byte.

    ldy #SLEEP_HIGH            ; Höherwertiges Byte (für 115200: 0)
    ldx #SLEEP_LOW             ; Niederwertiges Byte (für 115200: 1 -> 100µs)
	jsr __kernel_sleep
;    cli
    ply
	plx
    rts                       

;================================================================================
;   ACIA_send_string - Send null-terminated string
;   ————————————————————————————————————
;   Parameters:      ZP_ACIA_SPTR, ZP_ACIA_SPTR+1 string pointer
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
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
;   ACIA_ihandler - ACIA IRQ Handler
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
ACIA_ihandler:               ; IRQ handler for ACIA RX. Must be called by overall IRQ handler
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

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
@ACIA_ihandler_end:
    ply                       ; restore y
    plx                       ; restore x
    pla                       ; restore Akku
    rti
