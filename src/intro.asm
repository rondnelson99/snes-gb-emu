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

    ; dispatch a BGP write
    seta8
    lda.l GB_MEMORY + $FF47 ; get the value in rBGP
    tay
    ldx #$47 ; prep the handler
    jsl DispatchIOWrite

    ; dispatch an OBP0 write
    seta8
    lda.l GB_MEMORY + $FF48 ; get the value in rBGP
    tay
    ldx #$48 ; prep the handler
    jsl DispatchIOWrite



    seta8
    lda #5 << 3
    sta.l BG1SC
    lda #2
    sta.l BG12NBA

Zero:
    .db 0



















.ENDS

.SECTION "Gameboy image", BASE $80 SUPERFREE
GBImage:
    .INCBIN "res/gb.bin"
GBImageEnd:
.ENDS
