VIA_PORTB = $8000
VIA_PORTA = $8001
VIA_DDRB = $8002
VIA_DDRA = $8003
VIA_T1C_L = $8004
VIA_T1C_H = $8005
VIA_T1L_L = $8006
VIA_T1L_H = $8007
VIA_T2C_L = $8008
VIA_T2C_H = $8009
VIA_SR = $800a
VIA_ACR = $800b
VIA_PCR = $800c
VIA_IFR = $800d
VIA_IER = $800e
VIA_PORTA_2 = $800f

TIMER_INTVL     = $b400  ; set the timer to this (dezimal 46.080) if CPU is running at 1.8432 MHz
TIMER_COUNT     = $28    ; after this count (dezimal 40) of interrupts a full second is passed
TIMER_INT_COUNT = $00FD  ; memory address to store how often the via interrupt has been called
LEDS            = $00FC  ; status of the LEDs

via_setup:
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
  lda #$aa
  sta VIA_PORTB
  jsr delaylong

  lda #$55
  sta VIA_PORTB
  jsr delaylong

  dex
  bne loop
  ; jmp loop           ; endless loop
  lda #$00
  sta VIA_PORTB
  rts

delayshort:
  phx
  ldy #$02
xloopshort:
  ldx #$02
innershort:
  dex
  bne innershort
  dey
  bne xloopshort
  plx
  rts

delaylong:
  phx
  ldy #$ff
xlooplong:
  ldx #$ff
innerlong:
  dex
  bne innerlong
  dey
  bne xlooplong
  plx
  rts

via_setup_timer:
  lda #%11000000       ; setting bit 7 sets interrupts and bit 6 enables Timer 1
  sta VIA_IER
  lda #%01000000       ; continuous interrupts, no toggle on bit 7 of Port B (PB7)
  sta VIA_ACR
  
  lda #$28             ; initialize the interrupt counter
  sta TIMER_INT_COUNT  ; initialize the interrupt counter
  
  lda #%00001111       ; initialize the LEDS
  sta LEDS
  
  lda #<TIMER_INTVL    ; Load low byte of our 16-bit value
  sta VIA_T1C_L
  lda #>TIMER_INTVL    ; Load high byte of our 16-bit value
  sta VIA_T1C_H        ; This starts the timer running
  rts

VIA_IRQ:                     ; IRQ handler for VIA Timer. Must be called by overall IRQ handler
SERVICE_VIA:
        bit VIA_IFR          ; Bit 6 copied to overflow flag
        bvc SERVICE_VIA_END  ; Overflow clear, so not Timer1
        lda VIA_T1C_L        ; Clears the interrupt
        dec TIMER_INT_COUNT  ; Decrement interrupt counter
        bne SERVICE_VIA_END  ; not a full second passed yet
        lda #$28             ; restore interrupt counter
        sta TIMER_INT_COUNT
        lda LEDS             ; every second toggle the LEDS from 1111 0000 to 0000 1111 and vice versa
        eor #$FF
        sta LEDS
        sta VIA_PORTB
SERVICE_VIA_END:
        ply                       ; restore y
        plx                       ; restore x
        pla                       ; restore Akku
        rti
