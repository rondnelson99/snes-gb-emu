.INCLUDE "defines.asm"

; this routine translates Shadow OAM to SNES OAM, taking the place of OAM DMA
; currently, it is called by the "call" opcode when it detects a call to the OAM DMA routine

.SECTION "Translate OAM" BASE $80 SUPERFREE
 ; Translate the OAM
    ; put the 16-bit OAM address in Y

TranslateOAM:
    setaxy16
    ldy #SHADOWOAM_ADDR

    ; move the directpage to the PPU I/O area
    lda #$2100
    tad
    xba ; make sure the high byte is 0 for later
    ; set the destination address in OAM to $0000
    stz <OAMADDL
    seta8

    lda #$80
    sta.l INIDISP ; enable FBLANK

TransferSprite:
    ; transfer all 40 sprites in a loop
    lda.w GB_MEMORY + 1, y ; grab the X position
    sec
    sbc #8 ; adjust for the 8-pixel offset
    sta <OAMDATA
    lda.w GB_MEMORY + 0, y ; grab the Y position
    sec
    sbc #16 ; adjust for the 16-pixel offset
    sta <OAMDATA
    lda.w GB_MEMORY + 2, y ; grab the tile number
    lsr ; the bottom bit is handled using palettes
    sta <OAMDATA
    lda.w <GB_MEMORY + 3, y ; grab the attributes
    ror ; discard the bottom bit but include the carry flag for the lookup
    tax ; translate the attributes using a table
    lda.l OAMTranslationTable, x
    sta <OAMDATA
    
    ; increment the GB OAM pointer
    iny
    iny
    iny
    iny ; unfortunately, this seems to be our fastest option

    ; check whether we're done
    ; OAM is 160 bytes long
    tya
    cmp #160
    bne TransferSprite

    seta16
    ; fix the Direct Page
    lda #$4300 ; DMA registers
    tad

    ; return to the main loop
    setaxy8

     ; disable FBLANK
    lda #15 ; full brightness
    sta.l INIDISP
    jmp.l StartDispatchOpcode
.ENDS

.SECTION "OAM Flag Translation Table", BASE $80 SUPERFREE
/*
On GB, these are the meanings of the bits in the OAM attributes byte:
7: Priotity (0=above BG, 1=behind BG)
6: Y flip
5: X flip
4: Palette (0=OBP0, 1=OBP1)
0-3: usused on DMG
we rotate them right by 1 bit with bit 0 of the tile number at the top, which should get translated to bit 0 of the sprite palette

On SNES, these are the meanings of the bits in the OAM attributes byte:
7: Y flip
6: X flip
4-5: priority (We use 10 for now)
1-3: palette (0-7)
0: upper bit of tile number (unused)

The mapping is as follows:
GB 7(tile) -> SNES 1
GB 6(7) -> SNES 4 (for now)
GB 5(6) -> SNES 7
GB 4(5) -> SNES 6
GB 3(4) -> SNES 2

SNES bits 0, 3, and 5 are always 0
*/
OAMTranslationTable:
    .REPT 256 INDEX I
        .db %100000 | (I & $80) >> 6 | (I & $40) >> 2 | (I & $20) << 2 | (I & $10) << 2 | (I & $08) >> 1
    .ENDR
.ENDS
        