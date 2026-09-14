.include "cpu.inc"

.include "constants.inc"

.export __kernel_sleep

; Die Schleife im Makro SLEEP_FOR_100us braucht für die ersten LOOP_COUNT-1 Durchläufe je 5 Zyklen.
; Für den letzen Durchlauf nur 4 Zyklen (Sprungbefehl hat unterschiedliche Zyklen, je nach Verzweigung oder Weiterlaufen)
; Gesamtzyklen des Makros SLEEP_FOR_100us: 3 + 2 + (LOOP_COUNT-1)*5 + 4 + 4 = LOOP_COUNT * 5 + 8 Zyklen
; Ziel-Zyklen = 100µs * CPU_FREQUENCY (in MHz) - Bsp. 2MHz: 100 * 2 = 200 Zyklen
; LOOP_COUNT * 5 + 8 = 100 * CPU_FREQUENCY
; LOOP_COUNT = (100 * CPU_FREQUENCY - 8) / 5 = 20 * CPU_FREQUENCY - 1,6
; Rundungsfehler beachten: LOOP_COUNT = (100 * CPU_FREQUENCY - 8 + 4) / 5 ; +4 sorgt bei der Integer-Division durch 5 für ein sauberes Aufrunden

; Berechne die exakten Ziel-Zyklen für 100µs
TARGET_CYCLES = 100 * CPU_FREQUENCY

; Überprüfen, ob wir NOPs brauchen, um den 8-Bit-Überlauf (255) zu verhindern - (100 * 14MHz - 8 + 4) / 5 = 279,2 => 279 > 255 (Registerbreite)
; Bei 1280 Zyklen ist die Rechnung (1280 - 8 + 4) / 5 = 255,2 => 255 => noch ok
.if TARGET_CYCLES > 1280
    ; Bei 14 MHz (1400 Zyklen) brauchen wir Entlastung.
    ; Wir fügen 10 NOPs (20 Zyklen) ein.
    USE_NOPS = 1
    REMAINDER_CYCLES = TARGET_CYCLES - 20
.else
    USE_NOPS = 0
    REMAINDER_CYCLES = TARGET_CYCLES
.endif

; Berechne nun den LOOP_COUNT basierend auf den verbleibenden Zyklen.
; Formel: (Zyklen - Register-Overhead [8 Zyklen] + Aufrundungs-Puffer [4]) / 5
LOOP_COUNT = (REMAINDER_CYCLES - 8 + 4) / 5

.macro SLEEP_FOR_100us
    .local @SLEEP_FOR_100us_loop
    phx                         ; (3 cycles)

    ; Falls die CPU 13-14 MHz hat, verbrennen wir hier vorab 20 Zyklen
    .if USE_NOPS
        .repeat 10
            nop                 ; 10 * 2 Zyklen = 20 Zyklen
        .endrepeat
    .endif

	ldx #LOOP_COUNT             ; (2 cycles)  -  e.g. 38 is for CPU_FREQUENCY == 2MHz
@SLEEP_FOR_100us_loop:
    dex                         ; (2 cycles)
	bne @SLEEP_FOR_100us_loop   ; (3 cycles in loop, 2 cycles at end)
	plx                         ; (4 cycles)
.endmacro

.segment "CODE"

;================================================================================
;   __kernel_sleep - sleeps a while
;   Sleeps for about 100us, param times (e.g. y = 0, x = 5 => sleeps about 500us).
;   Maximum sleep is about 6,5s (y = 255, x = 255).
;   Macro SLEEP_100us is adjusted to CPU_FREQUENCY so the whole loop lasts about 100us
;   (5 + 198) * (256 * y + x) + (9 * y) + 5|8 + 6
;   @2MHz: y = 0, x = 1 => (203 + 11) * 0,0000005 = 107us  -  0,0000005s (500ns) => 1 cycle @ 2MHz
;   ————————————————————————————————————
;   Parameters:      .X, .Y - 16bit uint - high in y, low in x
;   Returned Values: none
;   Destroys:        .X, .Y
;   ————————————————————————————————————
;================================================================================
__kernel_sleep:
    cpx #0                        ; (2 cycles)
	bne @kernel_sleep             ; (3 cycles if X != 0, 2 cycles if X == 0)
	cpy #0                        ; (2 cycles)
	beq @kernel_sleep_end         ; (2 cycles if Y != 0, 3 if Y == 0)

@kernel_sleep:
    dex                           ; (2 cycles)
    SLEEP_FOR_100us
    bne @kernel_sleep             ; (3 cycles in loop, 2 cycles at end)
	cpy #0                        ; (2 cycles)
	beq @kernel_sleep_end         ; (2 cycles in loop, 3 at the end if Y == 0)
    dey                           ; (2 cycles)
    bne @kernel_sleep             ; (3 cycles in loop, 2 cycles at end)
		
@kernel_sleep_end:
    rts                           ; (6 cycles)
