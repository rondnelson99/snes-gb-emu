.include "include\defines.asm"

.macro GetOpcodeTableByte ARGS I ; gets the byte to put in the jump table for the given I/O index
.if I < 128 ; the first half uses a funny alternating system, but the second half doesn't. This avoids repetition in each half
    .if I # 2 == 0 ; if even
        .redef opcode_table_byte, (I & $7f) | $80 
    .else
        .redef opcode_table_byte, ((I + 32) & $7f) | $80 
    .endif
.else
    .redef opcode_table_byte, (I  & $7f) | $80 
.endif
.endm

.macro GetOpcodeTableAddress ARGS lowbyte ; gets the address that will be jumped to for the given I/O index
    GetOpcodeTableByte lowbyte + 1
    .redef temp, opcode_table_byte
    GetOpcodeTableByte lowbyte
    .redef opcode_table_address, temp << 8 | opcode_table_byte
.endm

.macro DispatchOpcode
    plx
    jmp (OpcodeJumpTable, x)
.endm


.SECTION "opcode jump table", BANK OPCODEBANK BASE $80 FREE
OpcodeJumpTable: ; contains a jump table rith relatively spaced-out entries
.repeat 257 INDEX tablebyte
    GetOpcodeTableByte tablebyte
    .db opcode_table_byte
.endr
.ENDS

/*.rept 256 INDEX opcode
    GetOpcodeTableAddress opcode
    .print HEX opcode_table_address, "\n"
.endr
*/

.SECTION "dispatch OPCODE write", BANK OPCODEBANK BASE $80 FREE 
; long jump to this to atart executing opcodes!
StartDispatchOpcode:
    DispatchOpcode
.ENDS

.macro ld_rr_d16 ARGS regpair
    seta16
    pla
    sta <regpair
    DispatchOpcode
.endm

.index 8
GetOpcodeTableAddress $01
.SECTION "ld bc, d16", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_BC:
    ld_rr_d16 GB_BC
.ENDS

GetOpcodeTableAddress $11
.SECTION "ld de, d16", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_DE:
    ld_rr_d16 GB_DE
.ENDS

GetOpcodeTableAddress $21
.SECTION "ld hl, d16", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL:
    ld_rr_d16 GB_HL
.ENDS

GetOpcodeTableAddress $31
.SECTION "ld sp, d16", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_SP:
    ld_rr_d16 GB_SP
.ENDS

GetOpcodeTableAddress $af
.SECTION "xor a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
XOR_A:
    ldy #0
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $32
.SECTION "ld (hl-), a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL_DEC_A: ; We'll assume this isn't used to access IO
    seta16
    sty (lobyte(GB_HL))
    dec <GB_HL
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $CB
.SECTION "CB prefix", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CB_PREFIX:
    plx
    jmp.l StartDispatchPrefix
.ENDS

GetOpcodeTableAddress $20
.SECTION "jr nz", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
JR_NZ:
    ldx <GB_ZEROFLAG
    beq @noJump
@jump
    seta8
    tsc
    sec ; this essentially increments the PC to step past the offset byte
    adc 1,s ; add the offset byte to the PC
    xba ; look at the high byte
    ; now test the sign
    plx
    bpl @branchForward
    ; if negative, dec the high byte (but also add the carry)
    adc #-1
    xba
    tcs
    DispatchOpcode
@branchForward
    ; if positive, only add the carry
    adc #0
    xba
    tcs
    DispatchOpcode


@noJump
    plx ; discard the offset byte
    DispatchOpcode
.ENDS

