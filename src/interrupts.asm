.INCLUDE "defines.asm"

; Interrupt reason definitions:
INTERRUPT_VBLANK = $00 ; for all VBlanks and VBlank ends
INTERRUPT_LCDSTAT = $02 ; these last two are only used for their respective GB interrupts
INTERRUPT_TIMER = $04
.EXPORT INTERRUPT_VBLANK, INTERRUPT_LCDSTAT, INTERRUPT_TIMER
; note that n flag clear means that this is a GB interrupt only

; this file contains the Vblank handler, which is catght by jr nz and halt in the CPU loop

; Set the IRQ Vector:
.SECTION "IRQ Vector", BANK 0 ORGA $FFEE FORCE ; Native Mode IRQ vector
IRQVector:
    .dw IRQHandler
.ENDS

NUM_VRAM_CHUNKS = 10 ; VRAM is divided into 10 1KB chunks, and one chunk is DMA'd per frame

    ; These routines each copy small 1K chunks of data from WRAM to VRAM using a macro
.macro DMAVRAMChunk ARGS src, dest, len, mode, incmode ; src is an Extended WRAM address, 
;dest is a 16-bit VRAM address, len is a 16-bit length, opts get written to DMAMODE, incmode gets written to PPUCTRL if provided
    
    ; assume a16, xy8
    ; the dma registers look like this:
    ; $43x0: DMAMODE: includes write pattern, bus direction, A bus increment
    ; $43x1: destination PPU register
    ; $43x2-3: source address
    ; $43x4: source bank
    ; $43x5-6: length
    .index 8
    seta8
    lda #$80
    sta.l INIDISP ; enable FBLANK

    seta16
    ; write mode and dest register at the same time
    lda # mode | <VMDATAL << 8
    sta <DMAMODE
    ; write source address
    lda # src
    sta <DMAADDR
    ; write the bank
    ldx # BANK(src)
    stx <DMAADDRBANK
    ; write the high byte of the length 
    .assert (len & $ff) == 0
    ldx #>len
    stx <DMALENHI
    ; write the VRAM destination address
    lda # dest
    sta.l VMADDL
    ; start the DMA transfer
    seta8

    .if NARGS == 5
        lda # incmode
        sta.l PPUCTRL
    .endif

    lda #$01
    sta.l COPYSTART

    ; disable FBLANK
    lda #15 ; full brightness
    sta.l INIDISP

    jmp CheckGBVblank

.endm

.SECTION "IRQ handler", BANK 0 BASE $80 FREE
IRQHandler:
    jmp.l LongIRQHandler
.ENDS

.SECTION "Long IRQ handler", BANK 0 BASE $80 SUPERFREE

; The main IRQ handler has three steps:
; 1: execute special tasks based on the interrupt type
; 2: emulate the steps the SM83 takes when handling an interrupt
; 3: prepare the next interrupt

.index 8
.accu 8
LongIRQHandler:
    ; save the accumulator
    sty <GB_A
    ; when handling this interrupt, the 65816 pushed 4 bytes: bank, pc, flags
    ; pop 4 bytes to fix the stack
    setaxy16
    ply
    ply
    setaxy8

    ; first, check the reason for this interrupt
    ldy <NEXT_INTERRUPT_REASON
    ; it it's zero (VBlank end), then we need to do some additoonal processing for the host
    bne InterruptIsNotVBlank

    ; check whether this is line 0 or 144
    ldx <VBLANK_INTERRUPT_SCANLINE
    beq @VBlankEnd

    ; This is a VBlank start:

    ; change the Vblank interrupt scanline to 0
    stz <VBLANK_INTERRUPT_SCANLINE

    /* ; Start the WY HDMA
    lda #%10 ; HDMA Channel 1
    sta.l HDMAEN*/

    ; call an appropriate DMA routine using a jump table
    ldx <VBLANK_COUNTER
    inx
    inx ; inc twice to step past a pointer
    cpx # NUM_VRAM_CHUNKS * 2
    bne @notlast
    ldx #0
@notlast
    stx <VBLANK_COUNTER
    jmp (DMAJumpTable, x)

@VBlankEnd
    ; change the Vblank interrupt scanline to 144
    ldx #144
    stx <VBLANK_INTERRUPT_SCANLINE
    ; VBlank end is not a GB interrupt, so we skip the next step
    bra PrepNextInterrupt

CheckGBVblank:
    ; for the beginning-of-VBlank interrupt, we need to check if this is a GB interrupt as well
    lda <GB_IME
    beq PrepNextInterrupt
    lda.w GB_MEMORY + $ffff ; rIE
    bit #%00000001
    beq PrepNextInterrupt

    ; if we get here, then we need to emulate the GB interrupt


InterruptIsNotVBlank:

EmulateInterruptDispatch:


    

    ; load this while x is sitll 8-bit
    ldx <NEXT_INTERRUPT_REASON

    setaxy16
    ; push the PC to the emulated stack
    ldy <GB_SP
    dey
    dey
    tsa
    sta.w GB_MEMORY,y
    sty <GB_SP

    ; grab the new PC value
    lda.l GBVectors, x
    tas

    ; we've processed this interrupt, now we need to prep the next one
PrepNextInterrupt:
    setaxy8
    ; set a high interrupt distance so it gets overwritten by anything
    lda #255
    sta <Scratchpad
    ; if IME=0 then there won't be any GB interrupts
    ldx <GB_IME
    beq @NoGBInterrupts
    ; we calculate the scanline for the next interrupt of each type, and remember the soonest one

    ; test the GB interrupts

    ; timer/joypad/serial are not implemented and VBlank is handled separately, so we jsut test for STAT

    ; for now, STAT only includes LYC

    ; check if the STAT interrupt is enabled
    lda.w GB_MEMORY + $ffff ; rIE
    and #%00000010
    beq @NoSTATInterrupt

    ; basically now there are 2 possibilities:
    ; No STAT interrupt, LYC interrupt (other sources not implemented)
    lda.w GB_MEMORY + $ff41 ; rSTAT

    ; check if we're using LYC
    bit #%01000000
    beq @NoSTATInterrupt

    ; subtract the scanline this interrupt fired on from LYC
    lda.w GB_MEMORY + $ff45 ; rLYC    
    sec
    sbc <NEXT_INTERRUPT_SCANLINE ; this is the current interrupt scanline since we haven't read it yet
    beq @NoSTATInterrupt ; if we're already on this scanline, thenm this shouldn't trigger until next frame
    sta <Scratchpad
    ; now SPad holds the number of scanlines until the next LYC interrupt
    ldy #INTERRUPT_LCDSTAT ; keep the winning interrupt type in Y

@NoSTATInterrupt
@NoGBInterrupts

    ; Check for the Vblank start/end interrupts
    
    lda <VBLANK_INTERRUPT_SCANLINE
    sec
    sbc <NEXT_INTERRUPT_SCANLINE
    cmp <Scratchpad 
    bcs @notVblank

    ; if VBlank starts sooner:
    sta <Scratchpad ; get the new scanline difference and interupt flag
    ldy #INTERRUPT_VBLANK

@notVblank

    ; now we have our interrupt scanline difference and type
    ; add the old interrupt scanline to get the next scanline

    lda <Scratchpad
    clc
    adc <NEXT_INTERRUPT_SCANLINE
    sta <NEXT_INTERRUPT_SCANLINE
    ; store the line in the SNES line interrupt reg
    sta.l VTIMEL
    ; store the interrupt type
    sty <NEXT_INTERRUPT_REASON

    ; ack this interrupt
    lda.l TIMEUP

    ; restore the accumulator
    ldy <GB_A

    ; now we return to the CPU loop
    jmp.l StartDispatchOpcode

    
    ; DMA routines
DMASpriteTiles1:
    DMAVRAMChunk GB_VRAM_SPRITETILES, VRAM_SPRITE_TILES, $400, DMA_01, $80


DMASpriteTiles2:
    DMAVRAMChunk GB_VRAM_SPRITETILES + $400, VRAM_SPRITE_TILES + $200, $400, DMA_01


DMASpriteTiles3:
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_SPRITE_TILES + $400, $400, DMA_01


DMASpriteTiles4:
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_SPRITE_TILES + $600, $400, DMA_01


DMABGTiles1:
    DMAVRAMChunk GB_VRAM_BGTILES, VRAM_BG_TILES, $400, DMA_01


DMABGTiles2:
    DMAVRAMChunk GB_VRAM_BGTILES + $400, VRAM_BG_TILES + $200, $400, DMA_01


DMABGTiles3:
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_BG_TILES + $400, $400, DMA_01


DMABGTiles4:
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_BG_TILES + $600, $400, DMA_01


DMATilemap1:
    DMAVRAMChunk GB_VRAM_TILEMAP1, VRAM_BG_TILEMAP_1, $400, DMA_LINEAR, 0


DMATilemap2:
    DMAVRAMChunk GB_VRAM_TILEMAP2, VRAM_BG_TILEMAP_2, $400, DMA_LINEAR


DMAJumpTable:
    .dw DMASpriteTiles1
    .dw DMASpriteTiles2
    .dw DMASpriteTiles3
    .dw DMASpriteTiles4
    .dw DMABGTiles1
    .dw DMABGTiles2
    .dw DMABGTiles3
    .dw DMABGTiles4
    .dw DMATilemap1
    .dw DMATilemap2



.ENDS


.SECTION "GB Vectors", BASE $80 SUPERFREE
    ; holds the stack pointer values for the interrupt vectors
GBVectors:
    .dw $8040 - 1 ; VBLANK ; these have 8000 to account for stack ROM0 location and -1 to account for 65xx pull
    .dw $8048 - 1 ; STAT
    .dw $8050 - 1 ; TIMER
.ENDS






