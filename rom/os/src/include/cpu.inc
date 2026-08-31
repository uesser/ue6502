; .setcpu 65816 muss gesetzt werden, weil die Original 65C02 CPU den opcode wai nicht kannte,
; nur die 65C02W. Diese CPU kennt aber der ca65 nicht. Da 65C02W und 65816 opcode äquivalent sind
; ist es ok die 65816 hier zu setzen.
.setcpu "65816"
