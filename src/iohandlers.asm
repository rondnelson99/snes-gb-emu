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

;P1
GetIOTableAddress $00 ; P1
.SECTION "P1 Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
P1Handler:
    ; bit 4 means reading dpad, bit 5 means reading buttons
    seta8
    tya
    and #%00010000
    bne @ReadButtons ; 0=select
@ReadDPad
    lda.l JOY1H ; lower 4 bits are up, down, left, right
    ; we want down, up, left, right instead
    ; use a table to convert
    and #%00001111
    tax
    lda.l DPADP1Table, x
    sta.w GB_MEMORY + $FF00
    returnFromIOHandler
@ReadButtons
    ; we want start, select, b, a
    ; use bitshifts to arrange this
    lda #%0010 ; the top 4 bits
    sta.w GB_MEMORY + $FF00
    lda.l JOY1H ; the TOP 4 bits are b, y, select, start
    eor #$ff ; invert because SNES is active high
    lsr
    lsr
    lsr
    lsr
    
    lsr ; Start
    rol.w GB_MEMORY + $FF00
    lsr ; Select
    rol.w GB_MEMORY + $FF00
    lsr
    lsr ; B
    rol.w GB_MEMORY + $FF00

    lda.l JOY1L ; the TOP bit is A
    eor #$ff ; invert because SNES is active high
    asl
    rol.w GB_MEMORY + $FF00
    returnFromIOHandler
.ENDS

.SECTION "DPAD P1 Table", BASE $80 SUPERFREE
    ; index this table with JOY1H & $0f, get the rP1 value for the dpad
DPADP1Table:
    .rept 16 INDEX index
        .db ((index & %11) | ((index >> 1) & %100) | ((index << 1) & %1000)) ~ $0f | %10000
    .endr
.ENDS
    


GetIOTableAddress $0F ; Interrupt Flags
.SECTION "IF Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
IFHandler:
    returnFromIOHandler
.ENDS

; Window Registers
GetIOTableAddress $4A ; WY
.SECTION "WY Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
WYHandler:
    ; The implementation of WY is somewhat complicated. 
    ; It controls an HDMA Table which Contols the Window enable on BG1 (BG) and BG2 (Window)
    ; If WY != 0, then then the HDMA table is enabled. At first, we set BG0 fully windowed in and BG1 fully windowed out
    ; then we wait a number of lines controlled by WY and set up the Window according to WH0 set by WX
    tya
    beq @WYZero
    ; WY != 0
    ; NOT IMPLEMENTED
    brk $0
@WYZero
    ; WY == 0
    ; Check if Window is acutally enabled
    lda.w GB_MEMORY + $FF40 ; rLCDC
    bit #%00100000 ; window enable bit
    beq @done ; if it's not enabled, then we're done
    ; now we don't use the HDMA Table and simply configure the window directly
    lda.l W12SELatWY ; get the W12SEL value used by the window
    sta.l W12SEL ; Window 1 enable
    lda #$80
    sta.l WYHDMATable
@done    
    returnFromIOHandler
.ENDS

.RAMSECTION "WY table and variables" SLOT "LORAM"
WYHDMATable: ds 10 ; 10 bytes of HDMA table
W12SELLine0: ds 1 ; The value to write to W12SEL at the start of the frame
W12SELatWY: ds 1 ; The value written by the HDMA Table when WY is reached. Changes based on Winodow enable
.ENDS

GetIOTableAddress $4B ; WX
.SECTION "WX Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
WXHandler:
    ; The GB Window is Window 1 on the SNES. Therefore WX is the left edge of the SNES window
    tya
    sec
    sbc #7 ; subtract 7 to get the left edge
    sta.l WH0 ; Window 1 left edge
    returnFromIOHandler
.ENDS

GetIOTableAddress $45 ; LYC
.SECTION "LYC Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
LYCHandler:
    seta8
    tya ; write the value to the emulated memory
    sta.w GB_MEMORY + $FF45

    ; update the next interrupt

    ; TODO: Update the current scanline

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


    returnFromIOHandler
.ENDS

;STAT
GetIOTableAddress $41 ; STAT
.SECTION "STAT Handler", BANK IOHANDLERBANK BASE $80 ORGA io_table_address FORCE
STATHandler:
    sty.w GB_MEMORY + $FF41
    returnFromIOHandler
.ENDS

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

    ; for now, we'll only implement bits 3, 4, 5

    seta8
    tya

    ; store it in the GB memory
    sta.w GB_MEMORY + $FF40

; Bit 3: BG tile map location
    ldx # (VRAM_BG_TILEMAP_1 >> 10) << 2
    bit #%00001000 ; BG tile map
    beq @BGTilemap1
@BGTilemap2
    ldx # (VRAM_BG_TILEMAP_2 >> 10) << 2
@BGTilemap1
    ; store the tilemap address
    txa
    sta.l BG1SC

; Bit 6: Window tile map location
    ldx # (VRAM_BG_TILEMAP_1 >> 10) << 2
    bit #%01000000 ; Window tile map
    beq @WindowTilemap1
@WindowTilemap2
    ldx # (VRAM_BG_TILEMAP_2 >> 10) << 2
@WindowTilemap1
    ; store the tilemap address
    txa
    sta.l BG2SC

; Bit 4: BG/Window tile data location
    tya
    ldx # (VRAM_BG_TILES >> 12) | (VRAM_BG_TILES >> 8)
    bit #%00010000 ; BG tile data
    beq @BGTiles
@BGSharedTiles
    ldx # (VRAM_SPRITE_TILES >> 12) | (VRAM_SPRITE_TILES >> 8)
@BGTiles
    ; store the tile data address
    txa
    sta.l BG12NBA

; Bit 5: Window enable
    ; The HDMA Table that controls WY always runs, regardless of whether Window is enabled.
    ; That way, it can be enabled mid-frame
    ; This bit controls whether it writes a dummy value of zero to W12SEL or the actual value
    tya
    bit #%00100000 ; Window enable
    beq @WindowDisabled
    ; Window enabled
    ; Enable BG2 through TM
    lda #%10011 ; enable OBJ + BG1 and BG2
    sta.l TM ; write it to the TM register

     lda #%00110010 ; Use W1 for BG1 and BG2, invert it for BG2

    ; The other thing we need to do is check if we're past WY and enable it RIGHT NOW if we are
    ; Grab rLY for speed rather than reading from SNES registers
    ldx.w GB_MEMORY + $FF45 ; rLY
    ; check if we're past WY
    cpx.w GB_MEMORY + $FF4A ; rWY
    bcs @GotW12SEL

    sta.l W12SEL
    bra @GotW12SEL

@WindowDisabled
    ; disable BG2 through TM
    lda #%10001 ; enable OBJ + BG1 only
    sta.l TM ; write it to the TM register

    lda #0
    ; fall through
@GotW12SEL
    sta.l W12SELatWY ; store the value used by the HDMA Table


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
    ; save the accumulator
    sty <GB_A

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
    
    ; restore the accumulator
    ldy <GB_A
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

    ; save the accumulator
    sty <GB_A

    setaxy16
    .8BIT
    
    ; look up each color in the palette entry table
    ; palette to translate from is in y
    ; store the colors we get in 8 bytes of the Echo Scratchpad
    ; so we can move the direct page and still have faster access to them


    ; color 0 is ignored on GB - it's transparent
    ; unfortunately transparency only works if the ajdacent tile in VRAM is also transparent
    ; here we get a backup colow to use instead
    lda #COLOR_TRANSPARENT
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
    ; restore the accumulator
    ldy <GB_A
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


