.MEMORYMAP
	DEFAULTSLOT 0
	SLOT 0 START $8000 SIZE $8000 NAME "ROM"
	SLOT 1 START $0 SIZE $2000 NAME "LORAM"
	SLOT 2 START $FE2000 SIZE $1E000 NAME "WRAM"
.ENDME

.ROMBANKMAP
BANKSTOTAL 2
BANKSIZE $8000
BANKS 2
.ENDRO

.INCLUDE "snes.inc"

;;
; Sets the X and Y registers to use 8-bit values.
; The upper bits are treated as cleared.
.macro setxy8
  sep #$10
.endm

;;
; Sets the X and Y registers to use 16-bit values.
.macro setxy16
  rep #$10
.endm

;;
; Sets the accumulator (A) and memory-only data manipulation
; instructions (such as STZ and ROR) to use 8-bit data.
; The upper 8 bits of A can be swapped in and out with the
; XBA instruction.
.macro seta8
  sep #$20
.endm

;;
; Sets the accumulator and memory-only data manipulation
; instructions to use 16-bit data.
.macro seta16
  rep #$20
.endm

;;
; Equivalent to seta8 and setxy8.
.macro setaxy8
  sep #$30
.endm

;;
; Equivalent to seta16 and setxy16.
.macro setaxy16
  rep #$30
.endm
