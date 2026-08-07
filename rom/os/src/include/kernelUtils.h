.ifndef _KERNELUTILS_H_
_KERNELUTILS_H_ = 1

.macro SLEEP_FOR_100us
    .local @SLEEP_FOR_100us_loop
    phx                         ; (3 cycles)
	ldx #CPU_FREQUENCY * 19     ; (2 cycles)  -  e.g. 38 is for CPU_FREQUENCY == 2MHz
@SLEEP_FOR_100us_loop:
    dex                         ; (2 cycles)
	bne @SLEEP_FOR_100us_loop   ; (3 cycles in loop, 2 cycles at end)
	plx                         ; (4 cycles)
.endmacro

.endif