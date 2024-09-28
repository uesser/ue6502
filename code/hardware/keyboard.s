; PS/2 keyboard interface through VIA - needs PORTB necessarily
;

KEYB_DDR   = VIA_DDRB
KEYB_PORT  = VIA_PORTB
KEYB_ACR   = VIA_ACR
KEYB_PCR   = VIA_PCR
KEYB_SR    = VIA_SR
KEYB_IER   = VIA_IER
KEYB_IFR   = VIA_IFR
KEYB_T2C_L = VIA_T2C_L
KEYB_T2C_H = VIA_T2C_H

.macro KEYB_PREPARE_READ_CHARACTER
	; Start SR
	lda #$20
    sta KEYB_ACR
	lda #$2c
    sta KEYB_ACR
	lda KEYB_SR

	; Set T2 to interrupt after 11 bits
	lda #10
	sta KEYB_T2C_L
	stz KEYB_T2C_H
.endmacro

.macro KEYB_ADD_TO_BUFFER
	; Store a value in the buffer
	ldx ZP_KEYB_WR_PTR
	cpx ZP_KEYB_RD_PTR
	bne @keyb_add_to_buffer   ; if equal => buffer full

    ; TODO - bell 3 times (audio out)
    bra @keyb_add_to_buffer_end
	
@keyb_add_to_buffer:
	; Store the character and update the buffer pointer
	sta KEYB_BUFFER, x
	inx
    cpx #KEYB_BUFFER_SIZE
    bne @wr_keyb_buf_wr_ptr
    ldx #0
@wr_keyb_buf_wr_ptr:
    stx ZP_KEYB_WR_PTR

@keyb_add_to_buffer_end:
.endmacro

.macro WAITPB6LOW
    ldx #0
@wait_low:
    inx
;    beq @WAITPB6LOW_break
	bit KEYB_PORT
	bvs @wait_low
@WAITPB6LOW_break:
    ; X-Register is 0 on break loop => error - no keyboard
.endmacro

.macro WAITPB6HIGH
    ldx #0
@wait_high:
    inx
;    beq @WAITPB6HIGH_break
	bit KEYB_PORT
	bvc @wait_high
@WAITPB6HIGH_break:
    ; X-Register is 0 on break loop => error - no keyboard
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ps2_prepare_read_character:
	; Start SR
	lda #$20 + $00
    sta KEYB_ACR
	lda #$20 + $0c
    sta KEYB_ACR
	lda KEYB_SR     ; also sets back the interrupt flag in IFR
	
  ; Make a signal to trigger the oscilloscope
;	sta VIA_PORTA

	; Set T2 to interrupt after 11 bits
	lda #10
    sta KEYB_T2C_L
    stz KEYB_T2C_H
	rts

delay_ps2:
	phx
	ldx #0
@delayloop:
	nop
    dex
    bne @delayloop
	plx
	rts

waitpb6low:
	bit KEYB_PORT
    bvs waitpb6low
	rts

waitpb6high:
	bit KEYB_PORT
    bvc waitpb6high
	rts

ps2_add_to_buffer:
	; Store a value in the buffer
	ldx ZP_KEYB_WR_PTR
	cpx ZP_KEYB_RD_PTR
	bne @keyb_add_to_buffer   ; if equal => buffer full

    ; TODO - bell 3 times (audio out)
    bra @keyb_add_to_buffer_end
	
@keyb_add_to_buffer:
	; Store the character and update the buffer pointer
	sta KEYB_BUFFER, x
	inx
    cpx #KEYB_BUFFER_SIZE
    bne @wr_keyb_buf_wr_ptr
    ldx #0
@wr_keyb_buf_wr_ptr:
    stx ZP_KEYB_WR_PTR

@keyb_add_to_buffer_end:
    rts


;;;;; PS/2 initialisation

keyb_init:
	; Initialise the input buffer.  The empty state has the write pointer one ahead of the read pointer.
	lda #1
	sta ZP_KEYB_WR_PTR

	stz ZP_KEYB_RD_PTR
	
	; init CB1 and CB2
;	lda KEYB_PCR
;	ora #$10
;	and #$1f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 1 => CB1 = Positive Active Edge
;	sta KEYB_PCR

	; Set PB6 of KEYB port to input, so clock floats high
;	lda KEYB_DDR
;	and #$bf      ; set PB6 to low => PB6 is input
;	sta KEYB_DDR
    stz KEYB_DDR

	; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
	lda #$2c
	sta KEYB_ACR

	; Some USB-compatible keyboards dont act as PS/2 keyboards unless we send a reset command to them first
	lda #$ff
	jsr keyb_write
	cmp #$ff
	beq @no_keyb   ; accu = $ff means error, no keyboard present
	
	; Prepare for the first character
	jsr ps2_prepare_read_character

	; Disable interrupts except for T2 and SR
	lda #$7f
	sta KEYB_IER
	sta KEYB_IFR
	lda #$A4       ; $80 + $24
	sta KEYB_IER

    lda #0   ; no error

@no_keyb:
	rts


;;;;;; Write a byte to the PS/2 port - unbuffered


keyb_write:
    phx
	phy
    pha
	
	; Write a byte to the PS/2 port - bitbanging it for now, but it should be possible to use
	; the shift register

	; Pull clock low, pull data low, let clock go high, wait one tick
	; Then send data bits one per tick
	; Then send parity bit and stop bit
	; Then can read acknowledgement from device
	
	; Clock low, data low
;	lda KEYB_PORT
;	and #$bf
;	sta KEYB_PORT     ; set PB6 to low => set ps/2-clock to low
;	lda KEYB_DDR
;	ora #$40          ; set PB6 as output
;	sta KEYB_DDR
;	lda KEYB_PCR
;	ora #$c0          ; highest 3 bits (bit 7-5) set CB2 behaviour - 110 => Low output
;	sta KEYB_PCR
	stz KEYB_PORT
    ldx #$40           ; set PB6
    stx KEYB_DDR       ; as output
    lda KEYB_PCR       ; set CB2
	ora #$c0           ; to low output
    sta KEYB_PCR

	; Wait a while - at least 100us
;    ldy #0
;    ldx #1
;    jsr __kernel_sleep
	jsr delay_ps2

	; Let the clock float again
;	lda KEYB_DDR
;	and #$bf          ; set PB6 as input
;	sta KEYB_DDR
    stz KEYB_DDR

	; Track odd parity
	ldy #1

	; Loop once per bit
	ldx #8
	
	pla

@keyb_write_bitloop:
	; Send next bit
	rol
	jsr keyb_write_bit
;	cpx #$ff
;	beq @keyb_write_bitloop_error    ; error

	dex
	bne @keyb_write_bitloop

	; Send the parity bit
	tya
	ror
	jsr keyb_write_bit
;	cpx #$ff
;	beq @keyb_write_bitloop_error    ; error

	; Send the stop bit
	sec
	jsr keyb_write_bit
;	cpx #$ff
;	beq @keyb_write_bitloop_error    ; error

	; Wait one more time
	jsr keyb_write_bit
;	cpx #$ff
;	beq @keyb_write_bitloop_error    ; error

	; init CB1 and CB2
;	lda KEYB_PCR
;	and #$1f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 1 => CB1 = Positive Active Edge
;	sta KEYB_PCR
	
	lda #0
	ply
	plx
	rts

@keyb_write_bitloop_error:
    lda #$ff
	ply
	plx
	rts
	
keyb_write_bit:
	pha

	; The bit to write is in the carry

	; Default to pull CB2 low
	lda KEYB_PCR
	ora #$c0          ; highest 3 bits (bit 7-5) set CB2 behaviour - 110 => Low output

	; If next bit is clear, thats the right state for CB2
	bcc @keyb_write_bit_clear   ; bcc = branch on carry clear (carry = 0)

	; Otherwise track parity and let CB2 float instead
	iny

	and #$1f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 1 => CB1 = Positive Active Edge

@keyb_write_bit_clear:
	; Wait for one tick from the device
;    phx
;	WAITPB6HIGH
;	cpx #0
;	beq @keyb_write_bit_error      ; means X-Register is 0 => error, waited too long
;	WAITPB6LOW
;	cpx #0
;	beq @keyb_write_bit_error      ; means X-Register is 0 => error, waited too long
;    plx
	jsr waitpb6high
    jsr waitpb6low
    
	; Set the CB2 state
	sta KEYB_PCR

	pla
	rts

@keyb_write_bit_error:
    plx
    ldx #$ff

	pla
	rts


;;;;;; Interrupt handling for reading from PS/2 keyboard


KEYB_IRQ:
SERVICE_KEYB:
	; Check for VIA interrupts
	bit KEYB_IFR
	bmi irq_keyb
	rti

irq_keyb:
	pha

	; Check for PS/2 related VIA interrupts
	lda KEYB_IFR
	and #$24
	bne irq_keyb_ps2

	pla
	rti

irq_keyb_ps2:
	; It's either T2 or SR (shouldn't be both) - check for T2 first
	cmp #$20
	bcs irq_keyb_t2

	; Fall through to handle shift register interrupt

irq_keyb_sr:
	; Shift register interrupt happens after first 8 bits are read -
	; that is, a start bit and the first seven data bits	
	lda KEYB_SR
	
	; The start bit should have been zero
	bmi irq_keyb_framingerror

	sta ZP_KEYB_RD_RESULT
	
	pla
	rti

irq_keyb_t2:
	; T2 interrupt happens at the end of the character, read the last few bits, check parity, and add to buffer
    
    phx

	; Read the SR again
	lda KEYB_SR

	; The bottom bit is the stop bit, which should be set
	ror
	bcc irq_keyb_framingerror

	; Next is parity - then the last data bit.  Add the data bit to the result byte.
	; The parity will move to the bit 7 of A.
	ror
	ror
	rol ZP_KEYB_RD_RESULT

	; The bits of the result byte are now in reverse order - the non-IRQ code can deal with that though

	; Check the parity - it should be odd
	and #$80
	eor ZP_KEYB_RD_RESULT
	lsr
	eor ZP_KEYB_RD_RESULT
	sta ZP_KEYB_TMP
	lsr
	lsr 
	eor ZP_KEYB_TMP
	and #17
	beq irq_keyb_framingerror
	cmp #17
	beq irq_keyb_framingerror
	
	; No framing errors, and correct parity, so get ready for the next character, and store this one

	jsr ps2_prepare_read_character

	lda ZP_KEYB_RD_RESULT
	jsr ps2_add_to_buffer

	; Done
    plx
	pla
	rti

irq_keyb_framingerror:
    phy
    
	; Interrupt the device to resynchronise
	lda KEYB_DDR
	ora #$40
	sta KEYB_DDR     ; clock low

	; Wait a while - at least 100us
    ldy #0
    ldx #1
    jsr __kernel_sleep

	and #$bf
	sta KEYB_DDR     ; release clock

	; Prepare for the next character
	jsr ps2_prepare_read_character

	lda #$ff
	jsr ps2_add_to_buffer

    ply
    plx
	pla
	rti


;;;;;; Read a byte from the PS/2 buffer


keyb_buffer_read__wait:
    phy
@keyb_wait_for_scancode:	
	ldy ZP_KEYB_RD_PTR
	iny

	sei
	cpy ZP_KEYB_WR_PTR
	bne keyb_read_from_buffer_gotchar

	; The buffer is empty, wait for an interrupt
	wai
	cli
	bra @keyb_wait_for_scancode

keyb_read_from_buffer_gotchar:
	cli

	lda KEYB_BUFFER, y
	sty ZP_KEYB_RD_PTR

	; The bits are backwards because the PS/2 protocol and 6522 shift register work in opposite ways
    tay
	lda keycode_reverse_lookup_table, y

	ply
	rts


keycode_reverse_lookup_table:
  .byte $00, $80, $40, $c0, $20, $a0, $60, $e0
  .byte $10, $90, $50, $d0, $30, $b0, $70, $f0
  .byte $08, $88, $48, $c8, $28, $a8, $68, $e8
  .byte $18, $98, $58, $d8, $38, $b8, $78, $f8
  .byte $04, $84, $44, $c4, $24, $a4, $64, $e4
  .byte $14, $94, $54, $d4, $34, $b4, $74, $f4
  .byte $0c, $8c, $4c, $cc, $2c, $ac, $6c, $ec
  .byte $1c, $9c, $5c, $dc, $3c, $bc, $7c, $fc
  .byte $02, $82, $42, $c2, $22, $a2, $62, $e2
  .byte $12, $92, $52, $d2, $32, $b2, $72, $f2
  .byte $0a, $8a, $4a, $ca, $2a, $aa, $6a, $ea
  .byte $1a, $9a, $5a, $da, $3a, $ba, $7a, $fa
  .byte $06, $86, $46, $c6, $26, $a6, $66, $e6
  .byte $16, $96, $56, $d6, $36, $b6, $76, $f6
  .byte $0e, $8e, $4e, $ce, $2e, $ae, $6e, $ee
  .byte $1e, $9e, $5e, $de, $3e, $be, $7e, $fe
  .byte $01, $81, $41, $c1, $21, $a1, $61, $e1
  .byte $11, $91, $51, $d1, $31, $b1, $71, $f1
  .byte $09, $89, $49, $c9, $29, $a9, $69, $e9
  .byte $19, $99, $59, $d9, $39, $b9, $79, $f9
  .byte $05, $85, $45, $c5, $25, $a5, $65, $e5
  .byte $15, $95, $55, $d5, $35, $b5, $75, $f5
  .byte $0d, $8d, $4d, $cd, $2d, $ad, $6d, $ed
  .byte $1d, $9d, $5d, $dd, $3d, $bd, $7d, $fd
  .byte $03, $83, $43, $c3, $23, $a3, $63, $e3
  .byte $13, $93, $53, $d3, $33, $b3, $73, $f3
  .byte $0b, $8b, $4b, $cb, $2b, $ab, $6b, $eb
  .byte $1b, $9b, $5b, $db, $3b, $bb, $7b, $fb
  .byte $07, $87, $47, $c7, $27, $a7, $67, $e7
  .byte $17, $97, $57, $d7, $37, $b7, $77, $f7
  .byte $0f, $8f, $4f, $cf, $2f, $af, $6f, $ef
  .byte $1f, $9f, $5f, $df, $3f, $bf, $7f, $ff

; Some character codes...
; 
; A  0 0 0 1 1 1 0 0 0 0 1    ...    0 0 0 0 0 1 1 1 1 1 1     ...    0 0 0 1 1 1 0 0 0 0 1     ...
; B  0 1 0 0 0 1 1 0 0 0 1
; N  0 0 1 0 0 1 1 0 0 0 1
