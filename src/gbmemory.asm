.INCLUDE "defines.asm"

.RAMSECTION "GB memory" BASE $7F SLOT "EXRAM" 
GB_MEMORY: ds 0
GB_ROM: ds $8000
GB_VRAM: ds 0
GB_VRAM_SPRITETILES: ds $800
GB_VRAM_SHAREDTILES: ds $800
GB_VRAM_BGTILES: ds $800
GB_VRAM_TILEMAP1: ds $400
GB_VRAM_TILEMAP2: ds $400
GB_SRAM: ds $2000
GB_WRAM: ds $2000
GB_ECHO: ds $1F00
GB_IO: ds $80
GB_HRAM: ds $80
.ENDS

; define some areas of Echo RAM the emulator uses
.BASE $80

.ENUM $E000 EXPORT
EchoScratchpad: ds $10
.ENDE

