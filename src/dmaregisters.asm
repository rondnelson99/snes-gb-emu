.INCLUDE "defines.asm"
; The DMA registers are used as teh main directpage in this project
; some space needs to be reserved for the unusable DMA registers and for actual DMA
; in each channel's parameters, bytes C-F are ususable, so we reserve them
; DMA 0 is used for actual DMA, so we reserve the whole channel
.RAMSECTION "DMA Unusable 0" SLOT "DMA" ORGA $4300
DMA_UNUSABLE_0: ds 16 ; this is the whole channel
.ENDS

; the other channels only have the ususable bytes reserved
.RAMSECTION "DMA Unusable 1" SLOT "DMA" ORGA $431C 
DMA_UNUSABLE_1: ds 4
.ENDS

.RAMSECTION "DMA Unusable 2" SLOT "DMA" ORGA $432C
DMA_UNUSABLE_2: ds 4
.ENDS

.RAMSECTION "DMA Unusable 3" SLOT "DMA" ORGA $433C
DMA_UNUSABLE_3: ds 4
.ENDS

.RAMSECTION "DMA Unusable 4" SLOT "DMA" ORGA $434C
DMA_UNUSABLE_4: ds 4
.ENDS

.RAMSECTION "DMA Unusable 5" SLOT "DMA" ORGA $435C
DMA_UNUSABLE_5: ds 4
.ENDS

.RAMSECTION "DMA Unusable 6" SLOT "DMA" ORGA $436C
DMA_UNUSABLE_6: ds 4
.ENDS

.RAMSECTION "DMA Unusable 7" SLOT "DMA" ORGA $437C
DMA_UNUSABLE_7: ds 4
.ENDS


; now, we can actually reserve parts we want to use
; define an 8-byte scratchpad for anything we want to do
.RAMSECTION "Scratchpad" SLOT "DMA"
Scratchpad: ds 8
.ENDS
; define registers for the GB
.RAMSECTION "Registers" SLOT "DMA"
GB_A: db
GB_BC: dw
GB_DE: dw
GB_HL: dw
GB_SP: dw
GB_ZEROFLAG: db
GB_CARRYFLAG: db
GB_INVCARRY: db
.ENDS