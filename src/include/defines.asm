.MEMORYMAP
	DEFAULTSLOT 0
	SLOT 0 START $8000 SIZE $8000 NAME "ROM"
	SLOT 1 START $0 SIZE $2000 NAME "LORAM"
	SLOT 2 START $2000 SIZE $E000 NAME "HIRAM"
  SLOT 3 START $0 SIZE $10000 NAME "EXRAM"
  SLOT 4 START $4300 SIZE $80 NAME "DMA"
.ENDME

.ROMBANKMAP
BANKSTOTAL 4
BANKSIZE $8000
BANKS 4
.ENDRO

; define some constant banks
OPCODEBANK = 2
IOHANDLERBANK = 3


.SNESHEADER
ID "GBEM"
NAME "Game Boy Emulator"
LOROM
FASTROM
CARTRIDGETYPE $01
ROMSIZE $08
SRAMSIZE $03
COUNTRY $01
LICENSEECODE $00
VERSION $01

.ENDSNES

.INCLUDE "include/snes.inc"
.INCLUDE "include/config.inc"

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
