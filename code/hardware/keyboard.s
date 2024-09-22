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

.macro PS2_PREPARE_READ_CHARACTER
	; Start SR
	lda VIA_SR

	; Set T2 to interrupt after 11 bits
	lda #10
	sta VIA_T2CL
	stz VIA_T2CH
.endmacro

.macro PS2_ADD_TO_BUFFER
	; Store a value in the buffer
	ldx zp_ps2_bufwriteptr
	cpx zp_ps2_bufreadptr
	bne @ps2_add_to_buffer   ; if equal => buffer full

    ; TODO - bell 3 times (audio out)
    bra @ps2_add_to_buffer_end
	
@ps2_add_to_buffer:
	; Store the character and update the buffer pointer
	sta ps2_input_buffer, x
	inc zp_ps2_bufwriteptr

@ps2_add_to_buffer_end:
.endmacro

; e.g. If CPU_FREQUENCY = 2 (MHz) we loop 44 times => 
; 44 * 7 cycles (-1 at the end of the loop) + 2 fix cycles = 309 cycles => 309 * 0,0000005 (1 cycle = 5ns @ 2MHz) = 0,000154 = 154us
.macro PS2_DELAY   ; wait at least 100us
	ldx #CPU_FREQUENCY * 22                 ; 2
@ps2_delay_loop:
	nop                                     ;    2
	dex                                     ;    2
	bne @ps2_delay_loop                     ;    3 (while looping); 2 (at end of loop)
.endmacro

.macro WAITPB6LOW
    ldx #0
@wait_low:
    inx
    beq @WAITPB6LOW_break
	bit VIA_PORTB
	bvs @wait_low
@WAITPB6LOW_break:
    ; X-Register is 0 on break loop => error - no keyboard
.endmacro

.macro WAITPB6HIGH
    ldx #0
@wait_high:
    inx
    beq @WAITPB6HIGH_break
	bit VIA_PORTB
	bvc @wait_high
@WAITPB6HIGH_break:
    ; X-Register is 0 on break loop => error - no keyboard
.endmacro


;;;;; PS/2 initialisation

ps2_init:
	; Initialise the input buffer.  The empty state has the write pointer one ahead of the read pointer.
	lda #1
	sta zp_ps2_bufwriteptr

	stz zp_ps2_bufreadptr
	
	; init CB1 and CB2
	lda VIA_PCR
	and #$0f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 0 => CB1 = Negative Active Edge
	sta VIA_PCR

	; Set PB6 of VIA port B to input, so clock floats high
	lda VIA_DDRB
	and #$bf      ; set PB6 to low => PB6 is input
	sta VIA_DDRB

	; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
	lda #$2c
	sta VIA_ACR

	; Disable interrupts except for T2 and SR
	lda #$7f
	sta VIA_IER
	sta VIA_IFR
	lda #$80 + $24
	sta VIA_IER

	; Prepare for the first character
	PS2_PREPARE_READ_CHARACTER   ; macro

	; Some USB-compatible keyboards dont act as PS/2 keyboards unless we send a reset command to them first
	lda #$ff
	jsr ps2_write
	cmp #$ff
	beq @no_keyb   ; accu = $ff means error, no keyboard present
	
	; TODO - check for ACK (fa) and BAT/SelfTest (aa) from keyboard after we sent ff
		; FA - Acknowledge
		; AA - BAT / Self Test Passed
		; EE - Echo response
		; FE - Resend request
		; 00 - Error
		; FF - Error
    


    lda #0   ; no error

@no_keyb:
	rts


;;;;;; Write a byte to the PS/2 port - unbuffered


ps2_write:
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
	lda VIA_DDRB
	ora #$40          ; set PB6 as output
	sta VIA_DDRB
	lda VIA_PORTB
	and #$bf
	sta VIA_PORTB     ; set PB6 to low => set ps/2-clock to low
	lda VIA_PCR
	ora #$c0          ; highest 3 bits (bit 7-5) set CB2 behaviour - 110 => Low output ; bit 4 = 0 => CB1 = Negative Active Edge
	and #$cf          ; clear bit 5 and 4
	sta VIA_PCR

	; Wait a while - at least 100us
	PS2_DELAY        ; macro

	; Let the clock float again
	lda VIA_DDRB
	and #$bf          ; set PB6 as input
	sta VIA_DDRB

	; Track odd parity
	ldy #1

	; Loop once per bit
	ldx #8
	
	pla

ps2_write_bitloop:
	; Send next bit
	rol
	jsr ps2_write_bit
	cpx #$ff
	beq @ps2_write_bitloop_error    ; error

	dex
	bne ps2_write_bitloop

	; Send the parity bit
	tya
	ror
	jsr ps2_write_bit
	cpx #$ff
	beq @ps2_write_bitloop_error    ; error

	; Send the stop bit
	sec
	jsr ps2_write_bit
	cpx #$ff
	beq @ps2_write_bitloop_error    ; error

	; Wait one more time
	jsr ps2_write_bit
	cpx #$ff
	beq @ps2_write_bitloop_error    ; error

	; init CB1 and CB2
	lda VIA_PCR
	and #$0f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 0 => CB1 = Negative Active Edge
	sta VIA_PCR
	
	lda #0
	ply
	plx
	rts

@ps2_write_bitloop_error:
    lda #$ff
	ply
	plx
	rts
	
ps2_write_bit:
	pha

	; The bit to write is in the carry

	; Default to pull CB2 low
	lda VIA_PCR
	ora #$c0          ; highest 3 bits (bit 7-5) set CB2 behaviour - 110 => Low output ; bit 4 = 0 => CB1 = Negative Active Edge
	and #$cf          ; clear bit 5 and 4

	; If next bit is clear, thats the right state for CB2
	bcc @ps2_write_bit_clear   ; bcc = branch on carry clear (carry = 0)

	; Otherwise track parity and let CB2 float instead
	iny

	lda VIA_PCR
	and #$0f   ; highest 3 bits (bit 7-5) set CB2 behaviour - 000 => Input-negative active edge; bit 4 = 0 => CB1 = Negative Active Edge

@ps2_write_bit_clear:
	; Wait for one tick from the device
    phx
	WAITPB6HIGH
	beq @ps2_write_bit_error      ; means X-Register is 0 => error, waited too long
	WAITPB6LOW
	beq @ps2_write_bit_error      ; means X-Register is 0 => error, waited too long
    plx
    
	; Set the CB2 state
	sta VIA_PCR

	pla
	rts

@ps2_write_bit_error:
    plx
    ldx #$ff

	pla
	rts


;;;;;; Interrupt handling for reading from PS/2 keyboard


KEYB_IRQ:
SERVICE_KEYB:
	; Check for VIA interrupts
	bit VIA_IFR
	bmi irq_via
	rti

irq_via:
	pha

	; Check for PS/2 related VIA interrupts
	lda VIA_IFR
	and #$24
	bne irq_via_ps2

	pla
	rti

irq_via_ps2:
	; It's either T2 or SR (shouldn't be both) - check for T2 first
	cmp #$20
	bcs irq_via_ps2_t2

	; Fall through to handle shift register interrupt

irq_via_ps2_sr:
	; Shift register interrupt happens after first 8 bits are read -
	; that is, a start bit and the first seven data bits	
	lda VIA_SR
	
	; The start bit should have been zero
	bmi irq_via_ps2_framingerror

	sta zp_ps2_readresult
	
	pla
	rti

irq_via_ps2_t2:
	; T2 interrupt happens at the end of the character, read the last few bits, check parity, and add to buffer
    
    phx

	; Read the SR again
	lda VIA_SR

	; The bottom bit is the stop bit, which should be set
	ror
	bcc irq_via_ps2_framingerror

	; Next is parity - then the last data bit.  Add the data bit to the result byte.
	; The parity will move to the bit 7 of A.
	ror
	ror
	rol zp_ps2_readresult

	; The bits of the result byte are now in reverse order - the non-IRQ code can deal with that though

	; Check the parity - it should be odd
	and #$80
	eor zp_ps2_readresult
	lsr
	eor zp_ps2_readresult
	sta zp_temp
	lsr
	lsr 
	eor zp_temp
	and #17
	beq irq_via_ps2_framingerror
	cmp #17
	beq irq_via_ps2_framingerror
	
	; No framing errors, and correct parity, so get ready for the next character, and store this one

	PS2_PREPARE_READ_CHARACTER   ; macro

	lda zp_ps2_readresult
	PS2_ADD_TO_BUFFER   ; macro

	; Done
    plx
	pla
	rti

irq_via_ps2_framingerror:
	; Interrupt the device to resynchronise
	lda #$40
	sta VIA_DDRB     ; clock low

	; Wait a while - at least 100us
	PS2_DELAY        ; macro

	lda #0
	sta VIA_DDRB     ; release clock

	lda #$ff
	PS2_ADD_TO_BUFFER   ; macro

	; Prepare for the next character
	PS2_PREPARE_READ_CHARACTER   ; macro

    plx
	pla
	rti


;;;;;; Read a byte from the PS/2 buffer


ps2_buffer_read__wait:
    phy
@ps2_wait_for_scancode:	
	ldy zp_ps2_bufreadptr
	iny

	sei
	cpy zp_ps2_bufwriteptr
	bne ps2_read_from_buffer_gotchar

	; The buffer is empty, wait for an interrupt
	wai
	cli
	bra @ps2_wait_for_scancode

ps2_read_from_buffer_gotchar:
	cli

	lda ps2_input_buffer, y
	sty zp_ps2_bufreadptr

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
