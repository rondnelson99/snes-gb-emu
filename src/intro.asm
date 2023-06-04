.include "defines.asm"


STACK_BOTTOM = $1FFF
CPUIO_BASE   = $4200
PPU_BASE     = $2100


.SECTION "Reset Vector", ORGA $FFFC FORCE
    .dw Intro
.ENDS

.SECTION "Intro", FREE
Intro:
    sei                ; turn off IRQs
    clc
    xce                ; turn off 6502 emulation mode
    cld                ; turn off decimal ADC/SBC
    jmp.l ResetFastROM
.ENDS

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
    .accu 16
    .index 8

    ; write mode and dest register at the same time
    lda # mode | <VMDATAL << 8
    sta DMAMODE
    ; write source address
    lda # src
    sta DMAADDR
    ; write the bank
    ldx # BANK(src)
    stx DMAADDRBANK
    ; write the high byte of the length 
    .assert (len & $ff) == 0
    ldx #>len
    stx DMALENHI
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

.endm

.SECTION "ResetFastROM", BASE $80 SUPERFREE
ResetFastROM:
    setaxy16
    ldx #STACK_BOTTOM
    txs ; set the stack pointer

    ; Initialize the CPU I/O registers to predictable values
    lda #CPUIO_BASE
    tad              ; temporarily move direct page to S-CPU I/O area
    lda #$FF00
    sta $00     ; disable NMI and HVIRQ; don't drive controller port pin 6
    stz $02     ; clear multiplier factors
    stz $04     ; clear dividend
    stz $06     ; clear divisor and low byte of hcount
    stz $08     ; clear high bit of hcount and low byte of vcount
    stz $0A     ; clear high bit of vcount and disable DMA copy
    stz $0C     ; disable HDMA and fast ROM

    ; Initialize the PPU registers to predictable values
  lda #PPU_BASE
  tad              ; temporarily move direct page to PPU I/O area

  ; first clear the regs that take a 16-bit write
  lda #$0080
  sta $00     ; Forced blank, brightness 0, sprite size 8/16 from VRAM $0000
  stz $02     ; OAM address = 0
  stz $05     ; BG mode 0, no mosaic
  stz $07     ; BG 1-2 map 32x32 from VRAM $0000
  stz $09     ; BG 3-4 map 32x32 from VRAM $0000
  stz $0B     ; BG tiles from $0000
  stz $16     ; VRAM address $0000
  stz $23     ; disable BG window
  stz $26     ; clear window 1 x range
  stz $28     ; clear window 2 x range
  stz $2A     ; clear window mask logic
  stz $2C     ; disable all layers on main and sub
  stz $2E     ; disable all layers on main and sub in window
  ldx #$0030
  stx $30     ; disable color math and mode 3/4/7 direct color
  ldy #$00E0
  sty $32     ; clear RGB components of COLDATA; disable interlace+pseudo hires

  ; now clear the regs that need 8-bit writes
  seta8
  sta $15     ; still $80: add 1 to VRAM pointer after high byte write
  stz $1A     ; enable mode 7 wrapping and disable flipping
  stz $21     ; set CGRAM address to color 0
  stz $25     ; disable obj and math window

  ; The scroll registers $210D-$2114 need double 8-bit writes
.rept 8 INDEX I
    stz $0D+I
    stz $0D+I
.endr

    
    ; As do the mode 7 registers, which we set to the identity matrix
    ; [ $0100  $0000 ]
    ; [ $0000  $0100 ]
    lda #$01
    stz $1B
    sta $1B
    stz $1C
    stz $1C
    stz $1D
    stz $1D
    stz $1E
    sta $1E
    stz $1F
    stz $1F
    stz $20
    stz $20

    ; Enable FastROM
    lda #$01
    sta MEMSEL

    ; DMA The GB image from ROM to WRAM
    ; set the DMA registers
    ; source address
    lda #BANK(GBRom)
    sta.w DMAADDRBANK
    ldx #GBRom
    stx.w DMAADDR
    ; dest address
    lda #<WMDATA
    sta.w DMAPPUREG
    ldx #$0000
    stx.w WMADDL
    lda #$7F ; bank
    sta.w WMADDH
    ; length
    ldx #GBRomEnd - GBRom
    stx.w DMALEN
    ; other properties
    stz.w DMAMODE ; default properties

    ; finally, start the DMA transfer
    lda #$01 ; Channel 0
    sta.w COPYSTART
    ; set the DMA registers
    ; source address
    lda #BANK(GBImage)
    sta.w DMAADDRBANK
    ldx #GBImage
    stx.w DMAADDR
    ; dest address
    lda #<WMDATA
    sta.w DMAPPUREG
    ldx #$8000
    stx.w WMADDL
    lda #$7F ; bank
    sta.w WMADDH

    ; length
    ldx #GBImageEnd - GBImage
    stx.w DMALEN
    ; other properties
    stz.w DMAMODE ; default properties

    ; finally, start the DMA transfer
    lda #$01 ; Channel 0
    sta.w COPYSTART

    ; Zero all of VRAM
    ; set the DMA registers
    ; source address
    lda #BANK(Zero)
    sta.w DMAADDRBANK
    ldx #Zero
    stx.w DMAADDR
    ; dest address
    lda #<VMDATAL
    sta.w DMAPPUREG
    ldx #0
    stx.w VMADDL

    ; length
    ldx #$8000
    stx.w DMALEN
    ; other properties
    lda # DMA_01 | DMA_CONST ; static PPU DMA
    sta.w DMAMODE
    lda #$01 ; Channel 0
    sta.w COPYSTART

    ; Zero all of OAM
    ; set the DMA registers
    ; source address
    lda #BANK(Zero)
    sta.w DMAADDRBANK
    ldx #Zero
    stx.w DMAADDR
    ; dest address
    lda #<OAMDATA
    sta.w DMAPPUREG
    ldx #0
    stx.w OAMADDL

    ; length
    ldx # 512 + 32
    stx.w DMALEN
    ; other properties
    lda # DMA_00 | DMA_CONST ; static OAM DMA
    sta.w DMAMODE
    lda #$01 ; Channel 0
    sta.w COPYSTART


    ; park the direct page in the DMA registers
    seta16
    setxy8
    lda #$4300
    tad

    DMAVRAMChunk GB_VRAM_SPRITETILES, VRAM_SPRITE_TILES, $400, DMA_01
    DMAVRAMChunk GB_VRAM_SPRITETILES + $400, VRAM_SPRITE_TILES + $200, $400, DMA_01
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_SPRITE_TILES + $400, $400, DMA_01
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_SPRITE_TILES + $600, $400, DMA_01
    DMAVRAMChunk GB_VRAM_BGTILES, VRAM_BG_TILES, $400, DMA_01
    DMAVRAMChunk GB_VRAM_BGTILES + $400, VRAM_BG_TILES + $200, $400, DMA_01
    DMAVRAMChunk GB_VRAM_SHAREDTILES, VRAM_BG_TILES + $400, $400, DMA_01
    DMAVRAMChunk GB_VRAM_SHAREDTILES+ $400, VRAM_BG_TILES + $600, $400, DMA_01
    ; set VRAM to increment on low byte
    DMAVRAMChunk GB_VRAM_TILEMAP1, VRAM_BG_TILEMAP_1, $400, DMA_LINEAR, 0
    DMAVRAMChunk GB_VRAM_TILEMAP2, VRAM_BG_TILEMAP_2, $400, DMA_LINEAR

    ; temporarily move the stack to Low RAM
    seta16
    setxy8
    lda #$1fff
    tas

    ; set the DBR to $7F
    ldx #$7F
    phx
    plb

    ; move the SP to start popping opcodes
    lda #$8000 - 1
    tcs

    ; jump to the opcode handler!
    jmp.l StartDispatchOpcode
    

/*    ; dispatch a BGP write
    seta8
    lda.l GB_MEMORY + $FF47 ; get the value in rBGP
    tay
    ldx #$47 ; prep the handler
    jsl DispatchIOWrite

    ; dispatch an OBP0 write
    seta8
    lda.l GB_MEMORY + $FF48 ; get the value in rOBP0
    tay
    ldx #$48 ; prep the handler
    jsl DispatchIOWrite

    ; dispatch an OBP1 write
    lda.l GB_MEMORY + $FF49 ; get the value in rOBP1
    tay
    ldx #$49 ; prep the handler
    jsl DispatchIOWrite



    seta8
    lda #5 << 3
    sta.l BG1SC
    lda #2
    sta.l BG12NBA


    ; Translate the OAM
    ; put the 16-bit OAM address in Y
    setaxy16
    ldy #GB_OAM

    ; move the directpage to the PPU I/O area
    lda #$2100
    tad
    xba ; make sure the high byte is 0 for later
    ; set the destination address in OAM to $0000
    stz <OAMADDL
    seta8
TransferSprite:
    ; transfer all 40 sprites in a loop
    lda.w GB_MEMORY + 1, y ; grab the X position
    sta <OAMDATA
    lda.w GB_MEMORY + 0, y ; grab the Y position
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
*/

Zero:
    .db 0




.ENDS

.SECTION "Gameboy image", BASE $80 SUPERFREE
GBImage:
    .INCBIN "res/gb.bin"
GBImageEnd:
.ENDS

.SECTION "Gameboy ROM Image", BASE $80 BANK 0 ORGA $8000 FORCE
GBRom:
    .INCBIN "res/DMG_ROM.bin"
GBRomEnd:
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
4-5: priority (TODO: research this)
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
        .db (I & $80) >> 6 | (I & $40) >> 2 | (I & $20) << 2 | (I & $10) << 2 | (I & $08) >> 1
    .ENDR
.ENDS
        
