.include "cpu.inc"

.include "linker_symbols.h"

.include "constants.inc"
.include "sysdata.inc"
.include "kernelUtils.inc"
.include "via.h"

.export VIA_PORTB
.export VIA_PORTA
.export VIA_DDRB
.export VIA_DDRA
.export VIA_T1C_L
.export VIA_T1C_H
.export VIA_T1L_L
.export VIA_T1L_H
.export VIA_T2C_L
.export VIA_T2C_H
.export VIA_SR
.export VIA_ACR
.export VIA_PCR
.export VIA_IFR
.export VIA_IER
.export VIA_PORTA_2

.export VIA_init

.export VIA_ihandler

VIA_PORTB   = __IO_START__ + $00    ; I/O Port B
VIA_PORTA   = __IO_START__ + $01    ; I/O Port A
VIA_DDRB    = __IO_START__ + $02    ; Data Direction Register B
VIA_DDRA    = __IO_START__ + $03    ; Data Direction Register A
VIA_T1C_L   = __IO_START__ + $04    ; T1/CB1 shift rate
VIA_T1C_H   = __IO_START__ + $05    ; T1/CB1 shift rate
VIA_T1L_L   = __IO_START__ + $06    ; T1 lower latch
VIA_T1L_H   = __IO_START__ + $07    ; T1 upper latch
VIA_T2C_L   = __IO_START__ + $08    ; T2/CB2 shift rate
VIA_T2C_H   = __IO_START__ + $09    ; T2/CB2 shift rate
VIA_SR      = __IO_START__ + $0a    ; Shift Register
VIA_ACR     = __IO_START__ + $0b    ; Auxiliary Control Register
VIA_PCR     = __IO_START__ + $0c    ; Peripheral Control Register
VIA_IFR     = __IO_START__ + $0d    ; Interrupt Flag Register
VIA_IER     = __IO_START__ + $0e    ; Interrupt Enable Register
VIA_PORTA_2 = __IO_START__ + $0f    ; Port A - no handshake

TIMER_INTVL     = $b400  ; set the timer to this (dezimal 46.080) if CPU is running at 1.8432 MHz
TIMER_COUNT     = $28    ; after this count (dezimal 40) of interrupts a full second is passed
LEDS            = $00FC  ; status of the LEDs

.segment "OS_CODE"

;================================================================================
;   VIA_init - initializes the VIA
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
VIA_init:
    pha
	phx
	phy
	
    lda #$ff           ; all pins to output
    sta VIA_DDRB

    ; lda #$aa
    ; sta VIA_PORTB
    ; lda #$55
    ; sta VIA_PORTB
    ; lda #$aa
    ; sta VIA_PORTB
    ; lda #$55
    ; sta VIA_PORTB

    ldx #$05
loop:
    phx
	
    lda #$aa
    sta VIA_PORTB
	ldy #>10000
	ldx #<10000
    jsr _kernel_sleep

    lda #$55
    sta VIA_PORTB
	ldy #>10000
	ldx #<10000
    jsr _kernel_sleep
	
	plx

    dex
    bne loop
    ; jmp loop           ; endless loop
    lda #$00
    sta VIA_PORTB
	
	jsr VIA_init_timer
	
	phy
	plx
	pla
    rts

;================================================================================
;   VIA_init_timer - initializes the VIA timer
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        .A
;   ————————————————————————————————————
;================================================================================
VIA_init_timer:
    lda #%11000000           ; setting bit 7 sets interrupts and bit 6 enables Timer 1
    sta VIA_IER
    lda #%01000000           ; continuous interrupts, no toggle on bit 7 of Port B (PB7)
    sta VIA_ACR
    
    lda #$28                 ; initialize the interrupt counter
    sta ZP_VIA_TIMER_INT_CNT ; initialize the interrupt counter
    
    lda #%00001111           ; initialize the LEDS
    sta LEDS
    
    lda #<TIMER_INTVL        ; Load low byte of our 16-bit value
    sta VIA_T1C_L
    lda #>TIMER_INTVL        ; Load high byte of our 16-bit value
    sta VIA_T1C_H            ; This starts the timer running
    rts

;================================================================================
;   VIA_ihandler - VIA IRQ Handler
;   ————————————————————————————————————
;   Parameters:      none
;   Returned Values: none
;   Destroys:        none
;   ————————————————————————————————————
;================================================================================
VIA_ihandler:                ; IRQ handler for VIA Timer. Must be called by overall IRQ handler
    bit VIA_IFR              ; Bit 6 copied to overflow flag
    bvc @VIA_ihandler_end    ; Overflow clear, so not Timer1
    lda VIA_T1C_L            ; Clears the interrupt
    dec ZP_VIA_TIMER_INT_CNT ; Decrement interrupt counter
    bne @VIA_ihandler_end    ; not a full second passed yet
    lda #$28                 ; restore interrupt counter
    sta ZP_VIA_TIMER_INT_CNT
    lda LEDS                 ; every second toggle the LEDS from 1111 0000 to 0000 1111 and vice versa
    eor #$FF
    sta LEDS
    sta VIA_PORTB

; ISRs called by main ISR are called by jmp, not jsr, to save cycles in ISR chain.
; So we must get back the saved registers and return with RTI (leave the ISR-chain), not RTS.
@VIA_ihandler_end:
    ply                      ; restore y
    plx                      ; restore x
    pla                      ; restore Akku
    rti
