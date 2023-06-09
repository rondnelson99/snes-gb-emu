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
.endr
*/

.SECTION "dispatch prefix opcode", BANK PREFIXBANK BASE $80 FREE 
; long jump to this to atart executing opcodes!
StartDispatchPrefix: ; jump here with opcode in X, accu in y
    jmp (PrefixJumpTable, x)
.ENDS

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

gb_bit $7C, 7, GB_HL + 1
