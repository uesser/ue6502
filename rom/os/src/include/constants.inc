CPU_FREQUENCY = 2   ; clock frequency in MHz for delay subroutine(s)
                    ; !!! watch out: MUST NOT be greater than 12 (MHz) because loop count is greater than 8bit for >12MHz (see kernelUtils.s) !!!

BASIC_ENTRY = $C836
