.INCLUDE "defines.asm"
; The DMA registers are used as teh main directpage in this project
; some space needs to be reserved for the unusable DMA registers and for actual DMA
; in each channel's parameters, bytes C-F are ususable, so we reserve them
; DMA 0 is used for actual DMA, so we reserve the whole channel
.RAMSECTION "DMA Unusable 0" SLOT "DMA" ORGA $4300 FORCE
DMA_UNUSABLE_0: ds 16 ; this is the whole channel
.ENDS

; the other channels only have the ususable bytes reserved
.RAMSECTION "DMA Unusable 1" SLOT "DMA" ORGA $431C FORCE
DMA_UNUSABLE_1: ds 4
.ENDS

.RAMSECTION "DMA Unusable 2" SLOT "DMA" ORGA $432C FORCE
DMA_UNUSABLE_2: ds 4
.ENDS

.RAMSECTION "DMA Unusable 3" SLOT "DMA" ORGA $433C FORCE
DMA_UNUSABLE_3: ds 4
.ENDS

.RAMSECTION "DMA Unusable 4" SLOT "DMA" ORGA $434C FORCE
DMA_UNUSABLE_4: ds 4
.ENDS

.RAMSECTION "DMA Unusable 5" SLOT "DMA" ORGA $435C FORCE
DMA_UNUSABLE_5: ds 4
.ENDS

.RAMSECTION "DMA Unusable 6" SLOT "DMA" ORGA $436C FORCE
DMA_UNUSABLE_6: ds 4
.ENDS

.RAMSECTION "DMA Unusable 7" SLOT "DMA" ORGA $437C FORCE
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
GB_IME: db ; nonzero means interrupts are enabled
.ENDS
; define flags for the emulator
.RAMSECTION "Flags" SLOT "DMA"
VBLANK_COUNTER: db ; counts the number of VBLANKs that have passed * 2 mod 10. used to DMA rotating VRAM chunks
NEXT_INTERRUPT_REASON: db ; set by the intrrupt handler.
; every interrupt is handled by scanline IRQs, so we need a separate mechanism to see which type of interrupt we have
NEXT_INTERRUPT_SCANLINE: db ; holds the scanline for the next interrupt sp it can be read
VBLANK_INTERRUPT_SCANLINE: db ; holds the scanline for the next VBLANK-relatedd interrupt
; either 0 (VBLANK end) or 144 (VBLANK start)
; this makes it easier to avoid missing the interrupts

; No longer used:
; INTERRUPT_RETURN_ADDRESS: ds 3 ; holds the 24-bit return address for the interrupt handler
; typically this is FinishJRNZ but it can be changed to something else

.ENDS