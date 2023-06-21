.INCLUDE "defines.asm"
; this file contains the Vblank handler, which is catght by jr nz in the CPU loop

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
    seta16

    ; disable FBLANK
    lda #15 ; full brightness
    sta.l INIDISP

.endm


.SECTION "Vblank handler", BASE $80 SUPERFREE
HandleVBlank:
    ; first, ack the vblank but setting the vblank flag
    ldx #$80
    stx <FLAG_VBLANK
    ; call an appropriate DMA routine using a jump table
    ldx <VBLANK_COUNTER
    inx
    inx ; inc twice to step past a pointer
    cpx # NUM_VRAM_CHUNKS * 2
    bne @notlast
    ldx #0
@notlast:
    stx <VBLANK_COUNTER
    seta16
    jmp (DMAJumpTable, x)

    ; DMA routines
DMASpriteTiles1:
    DMAVRAMChunk GB_VRAM_SPRITETILES, VRAM_SPRITE_TILES, $400, DMA_01, $80
    jmp.l FinishJRNZ

DMASpriteTiles2:
    DMAVRAMChunk GB_VRAM_SPRITETILES + $400, VRAM_SPRITE_TILES + $200, $400, DMA_01
    jmp.l FinishJRNZ

DMASpriteTiles3:
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_SPRITE_TILES + $400, $400, DMA_01
    jmp.l FinishJRNZ

DMASpriteTiles4:
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_SPRITE_TILES + $600, $400, DMA_01
    jmp.l FinishJRNZ

DMABGTiles1:
    DMAVRAMChunk GB_VRAM_BGTILES, VRAM_BG_TILES, $400, DMA_01
    jmp.l FinishJRNZ

DMABGTiles2:
    DMAVRAMChunk GB_VRAM_BGTILES + $400, VRAM_BG_TILES + $200, $400, DMA_01
    jmp.l FinishJRNZ

DMABGTiles3:
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_BG_TILES + $400, $400, DMA_01
    jmp.l FinishJRNZ

DMABGTiles4:
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_BG_TILES + $600, $400, DMA_01
    jmp.l FinishJRNZ

DMATilemap1:
    DMAVRAMChunk GB_VRAM_TILEMAP1, VRAM_BG_TILEMAP_1, $400, DMA_LINEAR, 0
    jmp.l FinishJRNZ

DMATilemap2:
    DMAVRAMChunk GB_VRAM_TILEMAP2, VRAM_BG_TILEMAP_2, $400, DMA_LINEAR
    jmp.l FinishJRNZ

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