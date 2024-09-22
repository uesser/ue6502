ACIA_DATA = $8400
ACIA_STATUS = $8401
ACIA_COMMAND = $8402
ACIA_CONTROL = $8403

;; Helper routines for the ACIA buffer
;; from http://wilsonminesco.com/6502interrupts/index.html
.macro wr_acia_buf     ; write into ACIA buf and handle pointer
                       ; Put value in increment pointer.
        ldx ZP_ACIA_WR_PTR
        sta ACIA_BUFFER, X
        inx
        cpx #ACIA_BUFFER_SIZE
        bne @wr_acia_buf_end
        ldx #0
@wr_acia_buf_end:
        stx ZP_ACIA_WR_PTR
.endmacro

.macro rd_acia_buf     ; read from ACIA buf and handle pointer
                       ; Read value and increment pointer.
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

acia_setup:
	; Polled 65c51 I/O routines. Delay routine from
	; http://forum.6502.org/viewtopic.php?f=4&t=2543&start=30#p29795
	
    stz ZP_ACIA_WR_PTR             ; initialize ACIA write pointer
    stz ZP_ACIA_RD_PTR             ; initialize ACIA read pointer

	stz ACIA_STATUS             ; write anything to status register for program reset
	lda #$1f                    ; %0001 1111 = 19200 Baud
		                          ;              External receiver
		                          ;              8 bit words
		                          ;              1 stop bit
	sta ACIA_CONTROL            ; set control register  
	lda #$09                    ; %0000 1001 = Receiver odd parity check
		                          ;              Parity mode disabled
		                          ;              Receiver normal mode
		                          ;              RTSB Low, trans IRQ disabled
		                          ;              Receiver IRQB enabled
		                          ;              Data terminal ready (DTRB low)
	sta ACIA_COMMAND            ; set control register  
	rts

ACIA_IRQ:                        ; IRQ handler for ACIA RX. Must be called by overall IRQ handler
SERVICE_ACIA:
        lda ACIA_STATUS
        ;and #$07                ; Check for errors.
        ;bne SERVICE_ACIA_END    ; Ignore errors.
        and #$08                 ; Check for RX byte available
        beq SERVICE_ACIA_END     ; No byte available.

        ; There is a byte to get.
        lda ACIA_DATA
        wr_acia_buf

        ; Check how many bytes in the buffer are used.
;        acia_buf_dif
;        cmp #$F0
;        bcc SERVICE_ACIA_END
        ; There are only 15 chars left - de-assert RTS
;        lda #$01
;        sta ACIA_COMMAND

SERVICE_ACIA_END:
        ply                       ; restore y
        plx                       ; restore x
        pla                       ; restore Akku
        rti

;; Get_Char - get a character from the serial port into A.
;; Set the carry flag if char is valid.
;; Return immediately with carry flag clear if no char available.
;; Uses: A (return value)
ACIA_Get_Char:
        sei
        ;;  Check to see if there is a character.
        acia_buf_dif
        beq ACIA_no_char_available
ACIA_char_available:
        ;; See if RTS should be asserted (low)
        ;; buffer bytes in use in A from above.
;        cmp #$E0
;        bcs ACIA_buf_full
;        lda #$09
;        sta ACIA_COMMAND
ACIA_buf_full:
        phx                         ; Reading from buffer messes with X.
        rd_acia_buf                 ; Get the character.
        plx
        ;; jsr ACIA_Send_Char          ; Echo
        sec                         ; Indicate it is valid.
        cli
        rts
ACIA_no_char_available:
        clc                         ; Indicate no char available.
        cli
        rts

;; Print a single character to the console.
;; Send_Char - send character in A out serial port.
;; Uses: A (original value restored)
ACIA_Send_Char:
        ;  sei
        ;  pha                      ; save A
ACIA_wait_txd_empty:
        ;  lda ACIA_STATUS          ; Read ACIA status register
        ;  and #$10
        ;  beq ACIA_wait_txd_empty
        ;  pla                      ; ELSE, restore ACCUMULATOR from STACK
        sta ACIA_DATA               ; Send the byte.
        jsr ACIA_delay              ; Required delay - Comment out for working 6551/65C51!     
        ;  cli
        rts                       

ACIA_delay:
  phy                         ; Save Y Reg
  phx                         ; Save X Reg
  ldy   #2                    ; Get delay value (clock rate in MHz 2 clock cycles)
@minidly:
  ldx   #$68                  ; Seed X reg
@delay_1:
  dex                         ; Decrement low index
  bne @delay_1                ; Loop back until done
  dey                         ; Decrease by one
  bne @minidly                ; Loop until done
  plx                         ; Restore X Reg
  ply                         ; Restore Y Reg
@delay_done:
  rts                         ; Delay done, return
