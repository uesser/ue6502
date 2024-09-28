.macro SLEEP_FOR_100us
    phx                         ; (3 cycles)
	ldx #38                     ; (2 cycles)  -  38 is for CPU_FREQUENCY == 2MHz
@SLEEP_FOR_100us_loop:
    dex                         ; (2 cycles)
	bne @SLEEP_FOR_100us_loop   ; (3 cycles in loop, 2 cycles at end)
	plx                         ; (4 cycles)
.endmacro

;;
; __kernel_sleep
;
; param: 16bit uint - high in y, low in x
;
; Sleeps for about 100us, param times (e.g. y = 0, x = 5 => sleeps about 500us). Maximum sleep is about 6,5s (y = 255, x = 255).
;
; Macro SLEEP_100us is adjusted to CPU_FREQUENCY so the whole loop lasts about 100us
;
; (5 + 198) * (256 * y + x) + (9 * y) + 5|8 + 6
; @2MHz: y = 0, x = 1 => (203 + 11) * 0,0000005 = 107us  -  0,0000005s (500ns) => 1 cycle @ 2MHz
;;
__kernel_sleep:
    cpx #0                        ; (2 cycles)
	bne @kernel_sleep             ; (3 cycles if X != 0, 2 cycles if X == 0)
	cpy #0                        ; (2 cycles)
	beq @kernel_sleep_end         ; (2 cycles if Y != 0, 3 if Y == 0)

@kernel_sleep:
    dex                           ; (2 cycles)
  	SLEEP_FOR_100us               ; (198 cycles)
    bne @kernel_sleep             ; (3 cycles in loop, 2 cycles at end)
	cpy #0                        ; (2 cycles)
	beq @kernel_sleep_end         ; (2 cycles in loop, 3 at the end if Y == 0)
    dey                           ; (2 cycles)
    bne @kernel_sleep             ; (3 cycles in loop, 2 cycles at end)
		
@kernel_sleep_end:
    rts                           ; (6 cycles)

