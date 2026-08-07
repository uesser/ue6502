.ifndef _KERNEL_H_
_KERNEL_H_ = 1

.import __RAM_START__
.import __RAM_SIZE__
.import __RAM0_START__

PROGRAM_START = __RAM_START__                   ; memory location for user programs
PROGRAM_END = __RAM_START__ + __RAM_SIZE__      ; End of RAM

.endif
