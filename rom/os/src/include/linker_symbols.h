.ifndef _LINKER_SYMBOLS_H_
_LINKER_SYMBOLS_H_ = 1

; Memory defines
.import __ZP_START__              ; This is set to the start of the memory area, $C000 in this example.
.import __ZP_SIZE__               ; The size of the area, here $0100.
.import __ZP_LAST__               ; This is NOT the same as START+SIZE. Instead, it is defined as the first address that is not used by data. If we don't define any segments for this area, the value will be the same as START.
.import __ZP_FILEOFFS__           ; The binary offset in the output file. This is not defined for relocatable output file formats (o65).

.import __STACK_START__
.import __STACK_SIZE__
.import __STACK_LAST__
.import __STACK_FILEOFFS__

.import __RAMLOW_START__
.import __RAMLOW_SIZE__
.import __RAMLOW_LAST__
.import __RAMLOW_FILEOFFS__

.import __RAM_START__
.import __RAM_SIZE__
.import __RAM_LAST__
.import __RAM_FILEOFFS__

.import __IO_START__
.import __IO_SIZE__
.import __IO_LAST__
.import __IO_FILEOFFS__

.import __ROM_START__
.import __ROM_SIZE__
.import __ROM_LAST__
.import __ROM_FILEOFFS__

; Segemnts defines
.import __BSS_LOAD__
.import __BSS_RUN__
.import __BSS_SIZE__

.import __DATA_LOAD__
.import __DATA_RUN__
.import __DATA_SIZE__

.endif
