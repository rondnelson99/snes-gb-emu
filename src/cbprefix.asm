.include "include\defines.asm"

.macro GetOpcodeTableByte ARGS I ; gets the byte to put in the jump table for the given I/O index
.if I < 128 ; the first half uses a funny alternating system, but the second half doesn't. This avoids repetition in each half
    .if I # 2 == 0 ; if even
        .redef opcode_table_byte, (I & $7f) | $80 
    .else
        .redef opcode_table_byte, ((I + 68) & $7f) | $80 
    .endif
.elif I == $FF ; the last entry is special to avoid $fffe
    .redef opcode_table_byte, $81
.else
    .redef opcode_table_byte, (I  & $7f) | $80 
.endif

.if opcode_table_byte == $80 ; this is a hack to leave the first 257 bytes of the bank free for the jump table
    .redef opcode_table_byte, $E9
.endif

.endm

.macro GetOpcodeTableAddress ARGS lowbyte ; gets the address that will be jumped to for the given I/O index
    GetOpcodeTableByte lowbyte + 1
    .redef temp, opcode_table_byte
    GetOpcodeTableByte lowbyte
    .redef opcode_table_address, temp << 8 | opcode_table_byte
.endm

.macro FinishPrefix
    jmp.l StartDispatchOpcode
.endm


.SECTION "opcode jump table", BANK PREFIXBANK BASE $80 FREE
PrefixJumpTable: ; contains a jump table rith relatively spaced-out entries
.repeat 257 INDEX tablebyte
    GetOpcodeTableByte tablebyte
    .db opcode_table_byte
.endr
.ENDS

/*.rept 256 INDEX opcode
    GetOpcodeTableAddress opcode
    .print HEX opcode_table_address, "\n"
.endr*/


.SECTION "dispatch prefix opcode", BANK PREFIXBANK BASE $80 FREE 
; long jump to this to atart executing opcodes!
StartDispatchPrefix: ; jump here with opcode in X, accu in y
    jmp (PrefixJumpTable, x)
.ENDS

;--------------------------
; rotate/shift
;--------------------------

.macro rlc_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "rlc \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RLC_R\@:
    seta8
    lda <reg
    asl a ; rotate a copy into carry
    rol <reg
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

rlc_r $00, GB_BC + 1
rlc_r $01, GB_BC
rlc_r $02, GB_DE + 1
rlc_r $03, GB_DE
rlc_r $04, GB_HL + 1
rlc_r $05, GB_HL

GetOpcodeTableAddress $06
.SECTION "rlc [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RLC_HL:
    seta8
    setxy16
    ldx <GB_HL
    lda.w GB_MEMORY,x
    asl a ; rotate a copy into carry
    rol.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $07
.SECTION "rlc a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RLC_A:
    seta8
    tya
    asl a ; rotate a copy into carry
    tya
    rol a
    tay
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS

.macro rrc_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "rrc \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RRC_R\@:
    seta8
    lda <reg
    ror a ; rotate a copy into carry
    ror <reg
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

rrc_r $08, GB_BC + 1
rrc_r $09, GB_BC
rrc_r $0A, GB_DE + 1
rrc_r $0B, GB_DE
rrc_r $0C, GB_HL + 1
rrc_r $0D, GB_HL

GetOpcodeTableAddress $0E
.SECTION "rrc [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RRC_HL:
    seta8
    setxy16
    ldx <GB_HL
    lda.w GB_MEMORY,x
    ror a ; rotate a copy into carry
    ror.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $0F
.SECTION "rrc a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RRC_A:
    seta8
    tya
    ror a ; rotate a copy into carry
    tya
    ror a
    tay
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS

.macro rl_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "rl \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RL_R\@:
    seta8
    rol <GB_CARRYFLAG
    rol <reg
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

rl_r $10, GB_BC + 1
rl_r $11, GB_BC
rl_r $12, GB_DE + 1
rl_r $13, GB_DE
rl_r $14, GB_HL + 1
rl_r $15, GB_HL

GetOpcodeTableAddress $16
.SECTION "rl [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RL_HL:
    seta8
    setxy16
    ldx <GB_HL
    rol <GB_CARRYFLAG
    rol.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $17
.SECTION "rl a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RL_A:
    seta8
    tya
    rol <GB_CARRYFLAG
    rol a
    ror <GB_CARRYFLAG
    tay
    FinishPrefix
.ENDS

.macro rr_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "rr \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RR_R\@:
    seta8
    rol <GB_CARRYFLAG
    ror <reg
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

rr_r $18, GB_BC + 1
rr_r $19, GB_BC
rr_r $1A, GB_DE + 1
rr_r $1B, GB_DE
rr_r $1C, GB_HL + 1
rr_r $1D, GB_HL

GetOpcodeTableAddress $1E
.SECTION "rr [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RR_HL:
    seta8
    setxy16
    ldx <GB_HL
    rol <GB_CARRYFLAG
    ror.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $1F
.SECTION "rr a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RR_A:
    seta8
    tya
    rol <GB_CARRYFLAG
    ror a
    ror <GB_CARRYFLAG
    tay
    FinishPrefix
.ENDS

.macro sla_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "sla \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SLA_R\@:
.IF CONFIG_SLA_R_CAN_SET_Z == 1
    seta8
    lda <reg
    asl a
    sta <reg
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    FinishPrefix
.ELSE
    seta8
    asl <reg
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDIF
.ENDS
.endm

sla_r $20, GB_BC + 1
sla_r $21, GB_BC
sla_r $22, GB_DE + 1
sla_r $23, GB_DE
sla_r $24, GB_HL + 1
sla_r $25, GB_HL

GetOpcodeTableAddress $26
.SECTION "sla [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SLA_HL:
    seta8
    setxy16
    ldx <GB_HL
    asl.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $27
.SECTION "sla a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SLA_A:
    seta8
    tya
    asl a
    ror <GB_CARRYFLAG
    tay
    FinishPrefix
.ENDS

.macro sra_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "sra \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRA_R\@:
    seta8
    lda <reg
    asl a ; shift a copy into carry
    ror <reg 
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

sra_r $28, GB_BC + 1
sra_r $29, GB_BC
sra_r $2A, GB_DE + 1
sra_r $2B, GB_DE
sra_r $2C, GB_HL + 1
sra_r $2D, GB_HL

GetOpcodeTableAddress $2E
.SECTION "sra [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRA_HL:
    seta8
    setxy16
    ldx <GB_HL
    lda.w GB_MEMORY,x
    asl a ; shift a copy into carry
    ror.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $2F
.SECTION "sra a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRA_A:
    seta8
    tya
    asl a ; shift a copy into carry
    tya
    ror a 
    ror <GB_CARRYFLAG
    tay
    FinishPrefix
.ENDS

.macro swap_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "swap \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SWAP_R\@:
    seta8
    ldx <reg
    lda.l SwapTable,x
    sta <reg
    FinishPrefix
.ENDS
.endm

swap_r $30, GB_BC + 1
swap_r $31, GB_BC
swap_r $32, GB_DE + 1
swap_r $33, GB_DE
swap_r $34, GB_HL + 1
swap_r $35, GB_HL

.SECTION "Swap Table", BASE $80 SUPERFREE
; swap is implemented as a lookup table for speed
; who cares about 256 bytes anyway
SwapTable:
.REPT 256 INDEX i
    .db (i >> 4) | (i << 4) & $FF
.ENDR
.ENDS


GetOpcodeTableAddress $36
.SECTION "swap [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SWAP_HL:
    seta8
    lda (<GB_HL)
    tax
    lda.l SwapTable,x
    sta (<GB_HL)
    FinishPrefix
.ENDS

GetOpcodeTableAddress $37
.SECTION "swap a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SWAP_A:
    seta8
    tyx
    lda.l SwapTable,x
    tay
    FinishPrefix
.ENDS

.macro srl_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "srl \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRL_R\@:
    seta8
    lsr <reg
    ror <GB_CARRYFLAG
    FinishPrefix
.ENDS
.endm

srl_r $38, GB_BC + 1
srl_r $39, GB_BC
srl_r $3A, GB_DE + 1
srl_r $3B, GB_DE
srl_r $3C, GB_HL + 1
srl_r $3D, GB_HL

GetOpcodeTableAddress $3E
.SECTION "srl [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRL_HL:
    seta8
    setxy16
    ldx <GB_HL
    lsr.w GB_MEMORY,x
    ror <GB_CARRYFLAG
    setaxy8
    FinishPrefix
.ENDS

GetOpcodeTableAddress $3F
.SECTION "srl a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SRL_A:
    seta8
    tya
    lsr a
    ror <GB_CARRYFLAG
    tay
    FinishPrefix
.ENDS

;--------------------------
; bit
;--------------------------

.index 8
.macro gb_bit ARGS opcode, bit, reg
GetOpcodeTableAddress opcode
.SECTION "bit \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
Bit\@:
    seta8
    lda <reg
    and #1 << bit
    sta <GB_ZEROFLAG
    FinishPrefix
.ENDS
.endm

gb_bit $40, 0, GB_BC + 1
gb_bit $41, 0, GB_BC
gb_bit $42, 0, GB_DE + 1
gb_bit $43, 0, GB_DE
gb_bit $44, 0, GB_HL + 1
gb_bit $45, 0, GB_HL

gb_bit $48, 1, GB_BC + 1
gb_bit $49, 1, GB_BC
gb_bit $4A, 1, GB_DE + 1
gb_bit $4B, 1, GB_DE
gb_bit $4C, 1, GB_HL + 1
gb_bit $4D, 1, GB_HL

gb_bit $50, 2, GB_BC + 1
gb_bit $51, 2, GB_BC
gb_bit $52, 2, GB_DE + 1
gb_bit $53, 2, GB_DE
gb_bit $54, 2, GB_HL + 1
gb_bit $55, 2, GB_HL

gb_bit $58, 3, GB_BC + 1
gb_bit $59, 3, GB_BC
gb_bit $5A, 3, GB_DE + 1
gb_bit $5B, 3, GB_DE
gb_bit $5C, 3, GB_HL + 1
gb_bit $5D, 3, GB_HL

gb_bit $60, 4, GB_BC + 1
gb_bit $61, 4, GB_BC
gb_bit $62, 4, GB_DE + 1
gb_bit $63, 4, GB_DE
gb_bit $64, 4, GB_HL + 1
gb_bit $65, 4, GB_HL

gb_bit $68, 5, GB_BC + 1
gb_bit $69, 5, GB_BC
gb_bit $6A, 5, GB_DE + 1
gb_bit $6B, 5, GB_DE
gb_bit $6C, 5, GB_HL + 1
gb_bit $6D, 5, GB_HL

gb_bit $70, 6, GB_BC + 1
gb_bit $71, 6, GB_BC
gb_bit $72, 6, GB_DE + 1
gb_bit $73, 6, GB_DE
gb_bit $74, 6, GB_HL + 1
gb_bit $75, 6, GB_HL

gb_bit $78, 7, GB_BC + 1
gb_bit $79, 7, GB_BC
gb_bit $7A, 7, GB_DE + 1
gb_bit $7B, 7, GB_DE
gb_bit $7C, 7, GB_HL + 1
gb_bit $7D, 7, GB_HL

.macro bit_hl ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "bit {bit}, [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
BIT_HL\@:
    seta8
    lda (<GB_HL)
    and #1 << bit
    sta <GB_ZEROFLAG
    FinishPrefix
.ENDS
.endm

bit_hl $46, 0
bit_hl $4E, 1
bit_hl $56, 2
bit_hl $5E, 3
bit_hl $66, 4
bit_hl $6E, 5
bit_hl $76, 6
bit_hl $7E, 7

.macro bit_a ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "bit {bit}, a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
BIT_A\@:
    seta8
    tya
    and #1 << bit
    sta <GB_ZEROFLAG
    FinishPrefix
.ENDS
.endm

bit_a $47, 0
bit_a $4F, 1
bit_a $57, 2
bit_a $5F, 3
bit_a $67, 4
bit_a $6F, 5
bit_a $77, 6
bit_a $7F, 7

; set

.macro set_r ARGS opcode, bit, reg
GetOpcodeTableAddress opcode
.SECTION "set {bit}, \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SET_R\@:
    seta8
    lda #1 << bit
    tsb <reg
    FinishPrefix
.ENDS
.endm

set_r $C0, 0, GB_BC + 1
set_r $C1, 0, GB_BC
set_r $C2, 0, GB_DE + 1
set_r $C3, 0, GB_DE
set_r $C4, 0, GB_HL + 1
set_r $C5, 0, GB_HL

set_r $C8, 1, GB_BC + 1
set_r $C9, 1, GB_BC
set_r $CA, 1, GB_DE + 1
set_r $CB, 1, GB_DE
set_r $CC, 1, GB_HL + 1
set_r $CD, 1, GB_HL

set_r $D0, 2, GB_BC + 1
set_r $D1, 2, GB_BC
set_r $D2, 2, GB_DE + 1
set_r $D3, 2, GB_DE
set_r $D4, 2, GB_HL + 1
set_r $D5, 2, GB_HL

set_r $D8, 3, GB_BC + 1
set_r $D9, 3, GB_BC
set_r $DA, 3, GB_DE + 1
set_r $DB, 3, GB_DE
set_r $DC, 3, GB_HL + 1
set_r $DD, 3, GB_HL

set_r $E0, 4, GB_BC + 1
set_r $E1, 4, GB_BC
set_r $E2, 4, GB_DE + 1
set_r $E3, 4, GB_DE
set_r $E4, 4, GB_HL + 1
set_r $E5, 4, GB_HL

set_r $E8, 5, GB_BC + 1
set_r $E9, 5, GB_BC
set_r $EA, 5, GB_DE + 1
set_r $EB, 5, GB_DE
set_r $EC, 5, GB_HL + 1
set_r $ED, 5, GB_HL

set_r $F0, 6, GB_BC + 1
set_r $F1, 6, GB_BC
set_r $F2, 6, GB_DE + 1
set_r $F3, 6, GB_DE
set_r $F4, 6, GB_HL + 1
set_r $F5, 6, GB_HL

set_r $F8, 7, GB_BC + 1
set_r $F9, 7, GB_BC
set_r $FA, 7, GB_DE + 1
set_r $FB, 7, GB_DE
set_r $FC, 7, GB_HL + 1
set_r $FD, 7, GB_HL

.macro set_hl ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "set {bit}, [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SET_HL\@:
    seta8
    lda (<GB_HL)
    ora #1 << bit
    sta (<GB_HL)
    FinishPrefix
.ENDS
.endm

set_hl $C6, 0
set_hl $CE, 1
set_hl $D6, 2
set_hl $DE, 3
set_hl $E6, 4
set_hl $EE, 5
set_hl $F6, 6
set_hl $FE, 7

.macro set_a ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "set {bit}, a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
SET_A\@:
    seta8
    tya
    ora #1 << bit
    tay
    FinishPrefix
.ENDS
.endm

set_a $C7, 0
set_a $CF, 1
set_a $D7, 2
set_a $DF, 3
set_a $E7, 4
set_a $EF, 5
set_a $F7, 6
set_a $FF, 7

; reset

.macro res_r ARGS opcode, bit, reg
GetOpcodeTableAddress opcode
.SECTION "res {bit}, \@", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RES_R\@:
    seta8
    lda #~(1 << bit)
    and <reg
    sta <reg
    FinishPrefix
.ENDS
.endm

res_r $80, 0, GB_BC + 1
res_r $81, 0, GB_BC
res_r $82, 0, GB_DE + 1
res_r $83, 0, GB_DE
res_r $84, 0, GB_HL + 1
res_r $85, 0, GB_HL

res_r $88, 1, GB_BC + 1
res_r $89, 1, GB_BC
res_r $8A, 1, GB_DE + 1
res_r $8B, 1, GB_DE
res_r $8C, 1, GB_HL + 1
res_r $8D, 1, GB_HL

res_r $90, 2, GB_BC + 1
res_r $91, 2, GB_BC
res_r $92, 2, GB_DE + 1
res_r $93, 2, GB_DE
res_r $94, 2, GB_HL + 1
res_r $95, 2, GB_HL

res_r $98, 3, GB_BC + 1
res_r $99, 3, GB_BC
res_r $9A, 3, GB_DE + 1
res_r $9B, 3, GB_DE
res_r $9C, 3, GB_HL + 1
res_r $9D, 3, GB_HL

res_r $A0, 4, GB_BC + 1
res_r $A1, 4, GB_BC
res_r $A2, 4, GB_DE + 1
res_r $A3, 4, GB_DE
res_r $A4, 4, GB_HL + 1
res_r $A5, 4, GB_HL

res_r $A8, 5, GB_BC + 1
res_r $A9, 5, GB_BC
res_r $AA, 5, GB_DE + 1
res_r $AB, 5, GB_DE
res_r $AC, 5, GB_HL + 1
res_r $AD, 5, GB_HL

res_r $B0, 6, GB_BC + 1
res_r $B1, 6, GB_BC
res_r $B2, 6, GB_DE + 1
res_r $B3, 6, GB_DE
res_r $B4, 6, GB_HL + 1
res_r $B5, 6, GB_HL

res_r $B8, 7, GB_BC + 1
res_r $B9, 7, GB_BC
res_r $BA, 7, GB_DE + 1
res_r $BB, 7, GB_DE
res_r $BC, 7, GB_HL + 1
res_r $BD, 7, GB_HL

.macro res_hl ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "res {bit}, [hl]", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RES_HL\@:
    seta8
    lda (<GB_HL)
    and #~(1 << bit)
    sta (<GB_HL)
    FinishPrefix
.ENDS
.endm

res_hl $86, 0
res_hl $8E, 1
res_hl $96, 2
res_hl $9E, 3
res_hl $A6, 4
res_hl $AE, 5
res_hl $B6, 6
res_hl $BE, 7

.macro res_a ARGS opcode, bit
GetOpcodeTableAddress opcode
.SECTION "res {bit}, a", BANK PREFIXBANK BASE $80 ORGA opcode_table_address FORCE
RES_A\@:
    seta8
    tya
    and #~(1 << bit)
    tay
    FinishPrefix
.ENDS
.endm

res_a $87, 0
res_a $8F, 1
res_a $97, 2
res_a $9F, 3
res_a $A7, 4
res_a $AF, 5
res_a $B7, 6
res_a $BF, 7

