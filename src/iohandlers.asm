.include "include\defines.asm"

.macro GetIOTableByte ARGS I ; gets the byte to put in the jump table for the given I/O index
    .if I == 0
        .redef io_table_byte, $00
    .elif I == 128
        .redef io_table_byte, $80
    .else
        .redef io_table_byte, I + 127
    .endif
.endm

.macro GetIOTableAddress ARGS lowbyte ; gets the sddress that will be jumped to for the given I/O index
    GetIOTableByte lowbyte + 1
    .redef temp, io_table_byte
    GetIOTableByte lowbyte
    .redef io_table_address, temp << 8 | io_table_byte
.endm

.SECTION "io jump table", BANK IOHANDLERBANK BASE $80 FREE
IOJumpTable: ; contains a jump table rith relatively spaced-out entries
.repeat 129 INDEX tablebyte
    GetIOTableByte tablebyte
    .db io_table_byte
.endr
.ENDS

.SECTION "dispatch IO write", BANK IOHANDLERBANK BASE $80 FREE 
; long jump to this on IO write with address in X and value in Y
DispatchIOWrite:
    jmp (IOJumpTable, x) ; nice and simple
.ENDS

; assume index registers are 8bit, with value to write in y
.macro returnFromIOHandler
    jmp.l StartDispatchOpcode
.endm

.macro skipIORegister args addr
GetIOTableAddress addr
.SECTION "skip IO register\@", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
skipIORegister\@:
    returnFromIOHandler
.ENDS
.endm

;----------------------------------------
; Sound registers (not implemented)
;----------------------------------------

; skip addresses $10-$3F
.rept $40 - $10 INDEX soundreg
    skipIORegister soundreg + $10
.endr


GetIOTableAddress $40 ; LCDC: The hard one
.SECTION "LCDC Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
LCDCHandler:
    ; The 8 bits of LCDC are:
    ; 7: Display enable
    ; 6: Window tile map: 0=9800, 1=9C00
    ; 5: Window enable
    ; 4: BG & Window tile data: 0=8800, 1=8000
    ; 3: BG tile map: 0=9800, 1=9C00
    ; 2: OBJ size: 0=8x8, 1=8x16
    ; 1: OBJ enable
    ; 0: BG & Window enable

    ; for now, we'll only implement bits 3 and 4

    seta8
    tya
    ldx # (VRAM_BG_TILEMAP_1 >> 10) << 2
    bit #%00001000 ; BG tile map
    beq @BGTilemap1
@BGTilemap2
    ldx # (VRAM_BG_TILEMAP_2 >> 10) << 2
@BGTilemap1
    ; store the tilemap address
    txa
    sta.l BG1SC

    tya
    ldx # VRAM_BG_TILES >> 12
    bit #%00010000 ; BG tile data
    beq @BGTiles
@BGSharedTiles
    ldx # VRAM_SPRITE_TILES >> 12
@BGTiles
    ; store the tile data address
    txa
    sta.l BG12NBA

    returnFromIOHandler
.ENDS

    




GetIOTableAddress $42 ; SCY
.SECTION "SCY Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
SCYHandler:
    seta8
    tya
    ; store the value into emulated memory
    sta.w GB_MEMORY + $FF42
    sta.l BG1VOFS ; write the value
    sta.l BG1VOFS ; write the second byte (doesn't matter in this case)
    returnFromIOHandler
.ENDS

GetIOTableAddress $43 ; SCX
.SECTION "SCX Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
SCXHandler:
    seta8
    tya
    sta.w GB_MEMORY + $FF43
    sta.l BG1HOFS ; write the value
    sta.l BG1HOFS ; write the second byte (doesn't matter in this case)
    returnFromIOHandler
.ENDS



GetIOTableAddress $47 ; BGP
.SECTION "BGP Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
BGPHandler:
.index 8
    seta16
    ; move the direct page to the PPU registers (which has the CGRAM port in it)
    lda #$2100
    tad

    seta8 ; make everything 8-bit
    ; set the CGRAM address
    ;lda #0 ; first palette entry (  already contains 0)
    sta <CGADD
    
    ; the bottom 2 bits of y are color 0
    tya
    and #%11
    ; grab the low byte of the palette entry
    tax
    lda.l PaletteEntriesLow, x
    ; write it
    sta <CGDATA
    ; high byte
    lda.l PaletteEntriesHigh, x
    sta <CGDATA
    ; color 1
    tya
    lsr
    lsr
    tay
    and #%11
    tax
    lda.l PaletteEntriesLow, x
    sta <CGDATA
    lda.l PaletteEntriesHigh, x
    sta <CGDATA
    ; color 2
    tya
    lsr
    lsr
    tay
    and #%11
    tax
    lda.l PaletteEntriesLow, x
    sta <CGDATA
    lda.l PaletteEntriesHigh, x
    sta <CGDATA
    ; color 3
    tya
    lsr
    lsr
    tax
    lda.l PaletteEntriesLow, x
    sta <CGDATA
    lda.l PaletteEntriesHigh, x
    sta <CGDATA
    ; fix the direct page
    seta16
    lda #$4300 ; back to the DMA registers
    tad
    returnFromIOHandler
.ENDS

.SECTION "BGP Palette entry tables", BASE $80 SUPERFREE
; index with a color number (0-3) to get the palette entry for that color
; separate tables for low and high bytes of the palette entry
PaletteEntriesLow:
    .db lobyte(COLOR_WHITE)
    .db lobyte(COLOR_LIGHT_GRAY)
    .db lobyte(COLOR_DARK_GRAY)
    .db lobyte(COLOR_BLACK)
PaletteEntriesHigh:
    .db hibyte(COLOR_WHITE)
    .db hibyte(COLOR_LIGHT_GRAY)
    .db hibyte(COLOR_DARK_GRAY)
    .db hibyte(COLOR_BLACK)
.ENDS

.MACRO HandleOBP ARGS TableAddress CGIndex 
    ; this is similar to the BGP handler, but his time we generate the colors ahead of time,
; and then make more complex arrangements of them
    setaxy16
    .8BIT
    
    ; look up each color in the palette entry table
    ; palette to translate from is in y
    ; store the colors we get in 8 bytes of the Echo Scratchpad
    ; so we can move the direct page and still have faster access to them


    ; color 0
    tya ; get the palette entry
    asl ; double for the word-sized table
    and #%110 ; mask down to just the color index
    tax ; use it as an index
    lda.l TableAddress, x ; get the palette entry
    sta.w EchoScratchpad ; store it

    ; color 1
    tya
    lsr
    and #%110
    tax
    lda.l TableAddress, x
    sta.w EchoScratchpad + 2

    ; color 2
    tya
    lsr
    lsr
    lsr
    tay
    and #%110
    tax
    lda.l TableAddress, x
    sta.w EchoScratchpad + 4

    ; color 3
    tya
    lsr
    lsr
    and #%110
    tax
    lda.l TableAddress, x
    sta.w EchoScratchpad + 6

    ; now all our colors are in the EchoScratchpad
    ; now we just write them to CGRAM in a special arrangement
    ; first we write:
    ; 0123012301230123 - to not care about the high 2 bits of the color index
    ; then we write:
    ; 0000111122223333 - to not care about the low 2 bits of the color index
    ; all in all, there's a lot of colors to write

    ; we'll DMA the data over, 8 bytes at a time
    seta8
    ldx # lobyte(CGDATA) << 8 | DMA_00
    stx <DMAMODE ; config and dest
    lda # BANK(GB_MEMORY)
    sta <DMAADDRBANK ; source bank
    ldx # EchoScratchpad
    stx <DMAADDR ; source address
    ldx # 8
    stx <DMALEN ; length

    ; set the address. W'll start with Sprite palette 0, which is at $80
    lda #CGIndex
    sta.l CGADD
    ; start the DMA
    lda #%1
    sta.l COPYSTART
    ; now, we just repeat 4 times, resetting the source address and length
    setaxy8
    ; x contains 8-bit length
    ldy # lobyte(EchoScratchpad)
    ;.assert lobyte(EchoScratchpad) == lobyte(EchoScratchpad + 8) ; make sure we didn't cross a page
    stx <DMALEN
    sty <DMAADDR
    sta.l COPYSTART
    stx <DMALEN
    sty <DMAADDR
    sta.l COPYSTART
    stx <DMALEN
    sty <DMAADDR
    sta.l COPYSTART

    ; now we write the pattern 0000111122223333
    ; this can't easily be done with DMA, so we'll just do it manually
    seta16
    lda #$2100 ; move the DP to the PPU registers
    tad
    ; we already have the correct CGRAM address
    ; color 0
    ldx.w EchoScratchpad
    ldy.w EchoScratchpad + 1
    .rept 4
        stx <CGDATA
        sty <CGDATA
    .endr
    ; color 1
    ldx.w EchoScratchpad + 2
    ldy.w EchoScratchpad + 3
    .rept 4
        stx <CGDATA
        sty <CGDATA
    .endr
    ; color 2
    ldx.w EchoScratchpad + 4
    ldy.w EchoScratchpad + 5
    .rept 4
        stx <CGDATA
        sty <CGDATA
    .endr
    ; color 3
    ldx.w EchoScratchpad + 6
    ldy.w EchoScratchpad + 7
    .rept 4
        stx <CGDATA
        sty <CGDATA
    .endr
    ; return the direct page to the DMA registers
    lda #$4300
    tad
    returnFromIOHandler
.ENDM

GetIOTableAddress $48 ; OBP0
.SECTION "OBP0 Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
OBP0Handler:
    HandleOBP PaletteEntriesOBP0, $80
.ENDS

GetIOTableAddress $49 ; OBP1
.SECTION "OBP1 Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
OBP1Handler:
    HandleOBP PaletteEntriesOBP1, $A0
.ENDS



.SECTION "OBP0 Palette entry tables", BASE $80 SUPERFREE
; here we use a single little-endian table for high and low bytes
; index with a color number (0-3) << 1 to get the palette entry for that color
PaletteEntriesOBP0:
    .dw COLOR_WHITE_OBP0
    .dw COLOR_LIGHT_GRAY_OBP0
    .dw COLOR_DARK_GRAY_OBP0
    .dw COLOR_BLACK_OBP0
.ENDS

.SECTION "OBP1 Palette entry tables", BASE $80 SUPERFREE
PaletteEntriesOBP1:
    .dw COLOR_WHITE_OBP1
    .dw COLOR_LIGHT_GRAY_OBP1
    .dw COLOR_DARK_GRAY_OBP1
    .dw COLOR_BLACK_OBP1
.ENDS


