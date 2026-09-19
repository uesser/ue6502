; PS/2 keyboard interface through VIA
;
; Install VIA card in I/O slot 1, with VIA in lower socket

.setcpu "65816"

.import __IO_START__

VIA_PORTB   = __IO_START__ + $00    ; I/O Port B
VIA_PORTA   = __IO_START__ + $01    ; I/O Port A
VIA_DDRB    = __IO_START__ + $02    ; Data Direction Register B
VIA_DDRA    = __IO_START__ + $03    ; Data Direction Register A
VIA_T1C_L   = __IO_START__ + $04    ; T1/CB1 shift rate
VIA_T1C_H   = __IO_START__ + $05    ; T1/CB1 shift rate
VIA_T1L_L   = __IO_START__ + $06    ; T1 lower latch
VIA_T1L_H   = __IO_START__ + $07    ; T1 upper latch
VIA_T2CL    = __IO_START__ + $08    ; T2/CB2 shift rate
VIA_T2CH    = __IO_START__ + $09    ; T2/CB2 shift rate
VIA_SR      = __IO_START__ + $0a    ; Shift Register
VIA_ACR     = __IO_START__ + $0b    ; Auxiliary Control Register
VIA_PCR     = __IO_START__ + $0c    ; Peripheral Control Register
VIA_IFR     = __IO_START__ + $0d    ; Interrupt Flag Register
VIA_IER     = __IO_START__ + $0e    ; Interrupt Enable Register
VIA_PORTA_2 = __IO_START__ + $0f    ; Port A - no handshake

zp_ps2_readresult = $20
zp_temp = $0

zp_ps2_bufreadptr = $30
zp_ps2_bufwriteptr = $31

ps2_input_buffer = $2000

irqentry: jmp irq
nmientry: jmp nmi

entry:
	ldx #$ff
    txs

	jsr printimm
	.byte "Checking VIA...", 10, 13, 0

	; Check VIA is present
	lda #$ff
    sta VIA_ACR
    cmp VIA_ACR
    bne novia
	lda #$00
    sta VIA_ACR
    cmp VIA_ACR
    bne novia

	jsr printimm
	.byte "VIA found", 10, 13, 0

	jmp viaok

novia:
	jsr printimm
	.byte "VIA not found", 10, 13, 0

stop:
	jmp stop


viaok:
	jsr ps2_init

	cli

mainloop:
	jsr ps2_read_from_buffer

	cmp #$ff
    beq report_framingerror

	jsr printhex
	jsr printspace
	jmp mainloop
	
report_framingerror:
	jsr printimm
	.byte "Framing error", 13, 10, 0
	jmp mainloop



;;;;; PS/2 initialisation


ps2_init:
	; Set VIA port B to all inputs, so clock floats high
	stz VIA_DDRB

	; Enable T2 counting pulses on PB6, and set SR in read mode, external clock (011)
	lda #$20 + $0c
    sta VIA_ACR

	; Some USB-compatible keyboards don't act as PS/2 keyboards unless we send a reset command to them first
	lda #$ff
    jsr ps2_write

	; Initialise input buffer
	jsr ps2_init_input_buffer

	; Prepare for the first character
	jsr ps2_prepare_read_character

	; Disable interrupts except for T2 and SR
	lda #$7f
    sta VIA_IER
    sta VIA_IFR
	lda #$80 + $24
    sta VIA_IER

	rts


ps2_prepare_read_character:
	; Start SR
	lda #$20 + $00
    sta VIA_ACR
	lda #$20 + $0c
    sta VIA_ACR
	lda VIA_SR
	sta VIA_PORTA

	; Set T2 to interrupt after 11 bits
	lda #10
    sta VIA_T2CL
    stz VIA_T2CH

	rts


;;;;;; Write a byte to the PS/2 port - unbuffered


ps2_write:
	; Write a byte to the PS/2 port - bitbanging it for now, but it should be possible to use
	; the shift register

	; Pull clock low, pull data low, let clock go high, wait one tick
	; Then send data bits one per tick
	; Then send parity bit and stop bit
	; Then can read acknowledgement from device
	
	; Clock low, data low
	stz VIA_PORTB
    ldx #$40
    stx VIA_DDRB
	ldx #$ca
    stx VIA_PCR

	; Wait a while
	jsr delay

	; Let the clock float again
	stz VIA_DDRB

	; Track odd parity
	ldy #1

	; Loop once per bit
	ldx #8

ps2_write_bitloop:
	; Send next bit
	lsr
	jsr ps2_write_bit

	dex
	bne ps2_write_bitloop

	; Send the parity bit
	tya
	ror
	jsr ps2_write_bit

	; Send the stop bit
	sec
	jsr ps2_write_bit

	; Wait one more time
	jsr ps2_write_bit

	; Make a signal to trigger the oscilloscope
	sta VIA_PORTA

	rts
	
ps2_write_bit:
	pha

	; The bit to write is in the carry

	; Default to pull CB2 low
	lda #$ca

	; If next bit is clear, that's the right state for CB2
	bcc ps2_write_bit_clear

	; Otherwise track parity and let CB2 float instead
	iny
	lda #$0a

ps2_write_bit_clear:
	; Wait for one tick from the device
	jsr waitpb6high
	jsr waitpb6low

	; Set the CB2 state
	sta VIA_PCR

	pla
	rts



;;;;;; Interrupt handling for reading from PS/2 keyboard


irq:
	; Ignore any pending serial interrupts
	stz VIA_IER

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
	phx
	phy

	; It's either T2 or SR (shouldn't be both) - check for T2 first
	cmp #$20
	bcs irq_via_ps2_t2

	; Fall through to handle shift register interrupt

irq_via_ps2_sr:
	; Shift register interrupt happens after first 8 bits are read -
	; that is, a start bit and the first seven data bits	
	lda VIA_SR
	sta VIA_PORTA
	sta zp_ps2_readresult+1
	
	; The start bit should have been zero
	bmi irq_via_ps2_framingerror

	sta zp_ps2_readresult
	ply
	plx
	pla
	rti

irq_via_ps2_t2:
	bit VIA_T2CL

	; T2 interrupt happens at the end of the character, read the last few bits, check parity, and add to buffer

	; Read the SR again
	lda VIA_SR
	sta VIA_PORTA
	sta zp_ps2_readresult+2

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

	jsr ps2_prepare_read_character

	lda zp_ps2_readresult
	jsr ps2_add_to_buffer

	; Synthesize framing errors sometimes ("B" key)
	lda zp_ps2_readresult
	cmp #$4c
	beq irq_via_ps2_causeframingerror

	; Done
	ply
	plx
	pla
	rti


irq_via_ps2_causeframingerror:
	; Create a framing error
	;jsr waitpb6high
    ;jsr waitpb6low
	;jsr ps2_prepare_read_character
	lda #8
	sta VIA_T2CL
	stz VIA_T2CH

	ply
	plx
	pla
	rti


irq_via_ps2_framingerror:
	; Interrupt the device to resynchronise
	lda #$40
	sta VIA_DDRB   ; clock low
	jsr delay                 ; at least 100us
	lda #0
	sta VIA_DDRB     ; release clock

	; Prepare for the next character
	jsr ps2_prepare_read_character

	lda #$ff
    jsr ps2_add_to_buffer

	ply
    plx
    pla
    rti


;;;;;; PS/2 input buffer management


ps2_init_input_buffer:
	; Initialise the input buffer.  The empty state has the write pointer one ahead of the read pointer.
	lda #1
    sta zp_ps2_bufwriteptr
	stz zp_ps2_bufreadptr

	rts

ps2_read_from_buffer:
	ldy zp_ps2_bufreadptr
	iny

	sei
	cpy zp_ps2_bufwriteptr
	bne ps2_read_from_buffer_gotchar

	; The buffer is empty, wait for an interrupt
	wai
	cli
	jmp ps2_read_from_buffer

ps2_read_from_buffer_gotchar:
	cli

	lda ps2_input_buffer,y
	sty zp_ps2_bufreadptr

	; The bits are backwards because the PS/2 protocol and 6522 shift register work in opposite ways
	phx
    phy
	tay
    and #$f
    tax
	tya
    and #$f0
    asl
    ora swapallbitslookup,x
	rol
    rol
    rol
    rol
	tay
    and #$f
    tax
	tya
    and #$f0
    ora swapallbitslookup,x
	ply
    plx
	rts

swapallbitslookup:
	.byte 0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15


ps2_add_to_buffer:
	; Store a value in the buffer
	ldy zp_ps2_bufwriteptr
	cpy zp_ps2_bufreadptr
	beq ps2_add_to_buffer_full ; no buffer space

	; Store the character and update the buffer pointer
	sta ps2_input_buffer,y
	iny
	sty zp_ps2_bufwriteptr

ps2_add_to_buffer_full:
	rts


; Some character codes...
; 
; A  0 0 0 1 1 1 0 0 0 0 1    ...    0 0 0 0 0 1 1 1 1 1 1     ...    0 0 0 1 1 1 0 0 0 0 1     ...
; B  0 1 0 0 0 1 1 0 0 0 1
; N  0 0 1 0 0 1 1 0 0 0 1


delay:
	phx
	ldx #0
delayloop:
	nop
    dex
    bne delayloop
	plx
	rts


waitpb6low:
	bit VIA_PORTB
    bvs waitpb6low
	rts

waitpb6high:
	bit VIA_PORTB
    bvc waitpb6high
	rts


printchar:
	sta VIA_SR
	rts


; Print the zero-terminated string placed immediately after the JSR.
; The return address is adjusted to skip over the string.
printimm:
	pla
	sta zp_temp
	pla
	sta zp_temp+1
	ldy #0
printimm_loop:
	lda (zp_temp),y
	beq printimm_done
	jsr printchar
	iny
	bra printimm_loop

printimm_done:
	; Include the terminator when advancing past the inline string.
	iny
	tya
	clc
	adc zp_temp
	sta zp_temp
	lda zp_temp+1
	adc #0
	pha
	lda zp_temp
	pha
	rts


printspace:
	lda #32
    jmp printchar

printhex:
	pha
	ror
    ror
    ror
    ror
	jsr print_nybble
	pla
print_nybble:
.scope
	pha
	and #15
	cmp #10
	bmi skipletter
	adc #6
skipletter:
	adc #48
	jsr printchar
	pla
	rts
.endscope


nmi:
	jsr printimm
	.byte "NMI", 0

	rti


codetop:

