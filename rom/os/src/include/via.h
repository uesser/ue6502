.ifndef _VIA_H_
_VIA_H_ = 1

.import __IO_START__

VIA_PORTB =    __IO_START__ + $00                    ; I/O Port B
VIA_PORTA =    __IO_START__ + $01                    ; I/O Port A
VIA_DDRB =     __IO_START__ + $02                    ; Data Direction Register B
VIA_DDRA =     __IO_START__ + $03                    ; Data Direction Register A
VIA_T1C_L =    __IO_START__ + $04                    ; T1/CB1 shift rate
VIA_T1C_H =    __IO_START__ + $05                    ; T1/CB1 shift rate
VIA_T1L_L =    __IO_START__ + $06                    ; T1 lower latch
VIA_T1L_H =    __IO_START__ + $07                    ; T1 upper latch
VIA_T2C_L =    __IO_START__ + $08                    ; T2/CB2 shift rate
VIA_T2C_H =    __IO_START__ + $09                    ; T2/CB2 shift rate
VIA_SR =       __IO_START__ + $0a                    ; Shift Register
VIA_ACR =      __IO_START__ + $0b                    ; Auxiliary Control Register
VIA_PCR =      __IO_START__ + $0c                    ; Peripheral Control Register
VIA_IFR =      __IO_START__ + $0d                    ; Interrupt Flag Register
VIA_IER =      __IO_START__ + $0e                    ; Interrupt Enable Register
VIA_PORTA_2 =  __IO_START__ + $0f                    ; Port A - no handshake

; Peripheral Control Register flags
VIA_PCR_CA1_INTERRUPT_NEGATIVE     = %00000000
VIA_PCR_CA1_INTERRUPT_POSITIVE     = %00000001
VIA_PCR_CA2_OUTPUT_HANDSHAKE       = %00001000
VIA_PCR_CA2_OUTPUT_PULSE           = %00001010
VIA_PCR_CB1_INTERRUPT_NEGATIVE     = %00000000
VIA_PCR_CB1_INTERRUPT_POSITIVE     = %00010000
VIA_PCR_CB2_OUTPUT_LOW             = %11000000
VIA_PCR_CB2_OUTPUT_HIGH            = %11100000

; Interrupt Enable Register flags
VIA_IER_CLEAR_FLAGS                = %00000000
VIA_IER_SET_FLAGS                  = %10000000
VIA_IER_CA1_FLAG                   = %00000010
VIA_IER_CA2_FLAG                   = %00000001

.endif
