.include "defines.asm"


STACK_BOTTOM = $1FFF
CPUIO_BASE   = $4200
PPU_BASE     = $2100

.SNESHEADER
ID "GBEM"
NAME "Game Boy Emulator"
LOROM
FASTROM
CARTRIDGETYPE $01
ROMSIZE $08
SRAMSIZE $03
COUNTRY $01
LICENSEECODE $00
VERSION $01

.ENDSNES

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

    ; DMA The VRAM image from ROM to WRAM

    ; set the DMA registers
    ; source address
    lda #BANK(VRAMImage)
    sta.w DMAADDRBANK
    ldx #VRAMImage
    stx.w DMAADDR
    ; dest address
    lda #<WMDATA
    sta.w DMAPPUREG
    ldx #$8000
    stx.w WMADDL
    lda #1 ; bank
    sta.w WMADDH

    ; length
    ldx #VRAMImageEnd - VRAMImage
    stx.w DMALEN
    ; other properties
    stz.w DMAMODE ; default properties

    ; finally, start the DMA transfer
    lda #$01 ; Channel 0
    sta.w COPYSTART














.ENDS

.SECTION "Gameboy VRAM image", BASE $80 SUPERFREE
VRAMImage:
    .INCBIN "res/vram.bin"
VRAMImageEnd:
.ENDS
