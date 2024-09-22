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

entry:
;	jsr printimm
;	.byte "Checking VIA...", 10, 13, 0
;
;	; Check VIA is present
;	lda #$ff : sta KEYB_ACR : cmp KEYB_ACR : bne novia
;	lda #$00 : sta KEYB_ACR : cmp KEYB_ACR : bne novia
;
;	jsr printimm
;	.byte "VIA found", 10, 13, 0
;
;	bra viaok

novia:
;	jsr printimm
;	.byte "VIA not found", 10, 13, 0

stop:
;	bra stop

viaok:
;	jsr ps2_init

;	cli

@loop:
;	jsr wait_for_ps2_read_from_buffer

;	cmp #$ff : beq report_framingerror

;	jsr printhex
;	jsr printspace
;	bra @loop
	
report_framingerror:
;	jsr printimm
;	.byte "Framing error", 13, 10, 0
;	bra @loop



;;;;; PS/2 initialisation


ps2_init:
	; Set VIA port B to all inputs, so clock floats high
	stz KEYB_DDR

	; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
	lda #$20 + $0c
    sta KEYB_ACR

	; Some USB-compatible keyboards dont act as PS/2 keyboards unless we send a reset command to them first
	lda #$ff
    jsr ps2_write

	; Initialise input buffer
	stz ZP_KEYB_RD_PTR
    stz ZP_KEYB_WR_PTR

	; Prepare for the first character
	jsr ps2_prepare_read_character

	; Disable interrupts except for T2 and SR
	lda #$7f
    sta KEYB_IER
    sta KEYB_IFR
	lda #$80 + $24
    sta KEYB_IER

	rts


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


;;;;;; Write a byte to the PS/2 port - unbuffered


ps2_write:
	; Write a byte to the PS/2 port - bitbanging it for now, but it should be possible to use the shift register

	; Pull clock low, pull data low, let clock go high, wait one tick
	; Then send data bits one per tick
	; Then send parity bit and stop bit
	; Then can read acknowledgement from device
  
	; Clock low, data low
	stz KEYB_PORT
  ldx #$40           ; set PB6
  stx KEYB_DDR       ; as output
  pha
  lda KEYB_PCR       ; set CB2
	ora #$c0           ; to low output
  sta KEYB_PCR
  pla
  
	; Wait a while
	jsr delay_ps2

	; Let the clock float again
	stz KEYB_DDR

	; Track odd parity
	ldy #1

	; Loop once per bit
	ldx #8

ps2_write_bitloop:
	; Send next bit
	rol
	jsr ps2_write_bit

	dex
	bne ps2_write_bitloop

	; Send the parity bit
	tya                     ; y-register to accumulator
  ror
	jsr ps2_write_bit

	; Send the stop bit
	sec                     ; set carry = 1
	jsr ps2_write_bit

	; Wait one more time
	jsr ps2_write_bit

	; Make a signal to trigger the oscilloscope
;	sta VIA_PORTA

	rts
	
ps2_write_bit:
	pha

	; The bit to write is in the carry

	; Default to pull CB2 low
  lda KEYB_PCR
	ora #$c0

	; If next bit is clear, that is the right state for CB2
	bcc ps2_write_bit_clear   ; bcc = branch on carry clear (carry = 0)

	; Otherwise track parity and let CB2 float instead
	iny
	and #$1f

ps2_write_bit_clear:
	; Wait for one tick from the device
	jsr waitpb6high
  jsr waitpb6low

	; Set the CB2 state
	sta KEYB_PCR

	pla
	rts



;;;;;; Interrupt handling for reading from PS/2 keyboard


KEYB_IRQ:
SERVICE_KEYB:
	; Check for VIA interrupts
	bit KEYB_IFR
	bmi irq_via

  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti

irq_via:
	; Check for PS/2 related VIA interrupts
	lda KEYB_IFR
	and #$24                  ; Timer-2 ($20) or ShiftRegister ($04) interrupt
	bne irq_via_ps2

  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti

irq_via_ps2:
	; It's either T2 or SR (shouldn't be both) - check for T2 first
	cmp #$20
	beq irq_via_ps2_t2

	; Fall through to handle shift register interrupt

irq_via_ps2_sr:
	; Shift register interrupt happens after first 8 bits are read -
	; that is, a start bit and the first seven data bits	
	lda KEYB_SR

	; Make a signal to trigger the oscilloscope
;	sta VIA_PORTA

	sta ZP_KEYB_RD_RESULT+1
	
	; The start bit should have been zero
	bmi irq_via_ps2_framingerror          ; bmi = branch if result negative => highest bit (start bit) is not 0

	sta ZP_KEYB_RD_RESULT

  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti

irq_via_ps2_t2:
	bit KEYB_T2C_L    ; clear interrupt flag in IFR

	; T2 interrupt happens at the end of the character, read the last few bits, check parity, and add to buffer

	; Read the SR again
	lda KEYB_SR
	
	; Make a signal to trigger the oscilloscope
;  sta VIA_PORTA

	sta ZP_KEYB_RD_RESULT+2

	; The bottom bit is the stop bit, which should be set
	ror
  bcc irq_via_ps2_framingerror    ; if carry is clear (stop bit = 0) => error

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
  beq irq_via_ps2_framingerror
	cmp #17
  beq irq_via_ps2_framingerror
	
	; No framing errors, and correct parity, so get ready for the next character, and store this one

	jsr ps2_prepare_read_character

	lda ZP_KEYB_RD_RESULT
	jsr ps2_add_to_buffer

	; Synthesize framing errors sometimes ("B" key)
	lda ZP_KEYB_RD_RESULT
	cmp #$4c
  beq irq_via_ps2_causeframingerror

	; Done
  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti

irq_via_ps2_causeframingerror:
	; Create a framing error
	;jsr waitpb6high
  ;jsr waitpb6low
	;jsr ps2_prepare_read_character
	lda #8
  sta KEYB_T2C_L
	stz KEYB_T2C_H

  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti

irq_via_ps2_framingerror:
	; Interrupt the device to resynchronise
	lda #$40                  ; PB6 as output
  sta KEYB_DDR              ; clock low
	jsr delay_ps2             ; at least 100us
	lda #0
  sta KEYB_DDR              ; release clock

	; Prepare for the next character
	jsr ps2_prepare_read_character

	lda #$ff
  jsr ps2_add_to_buffer

  ply                       ; restore y
  plx                       ; restore x
  pla                       ; restore Akku
	rti


;;;;;; PS/2 input buffer management


wait_for_ps2_read_from_buffer:
    phy
    phx

@wait_for_ps2:
	sei

	ldy ZP_KEYB_RD_PTR
	cpy ZP_KEYB_WR_PTR
	bne ps2_read_from_buffer_gotchar

	; The buffer is empty, wait for an interrupt
	wai
	cli
	jmp @wait_for_ps2

ps2_read_from_buffer_gotchar:
	cli

	ldx KEYB_BUFFER, y
    iny
    cpy #KEYB_BUFFER_SIZE
    bne @check_rd_size_end
    ldy #0
@check_rd_size_end:
	sty ZP_KEYB_RD_PTR

	; The bits are backwards because the PS/2 protocol and 6522 shift register work in opposite ways
    lda keycode_reverse_lookup_table, x
	
    plx
    ply
	rts

ps2_add_to_buffer:
	; Store a value in the buffer
	ldy ZP_KEYB_WR_PTR
;	cpy ZP_KEYB_RD_PTR
;	beq ps2_add_to_buffer_full ; no buffer space

	; Store the character and update the buffer pointer
	sta KEYB_BUFFER, y
	iny
    cpy #KEYB_BUFFER_SIZE
    bne @check_wr_size_end
    ldy #0
@check_wr_size_end:
	sty ZP_KEYB_WR_PTR

ps2_add_to_buffer_full:
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
