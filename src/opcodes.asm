.include "include\defines.asm"

.macro GetOpcodeTableByte ARGS I ; gets the byte to put in the jump table for the given I/O index
.if I < 128 ; the first half uses a funny alternating system, but the second half doesn't. This avoids repetition in each half
    .if I # 2 == 0 ; if even
        .redef opcode_table_byte, (I & $7f) | $80 
    .else
        .redef opcode_table_byte, ((I + 40) & $7f) | $80 
    .endif
.elif I == $FF ; the last entry is special to avoid $fffe
    .redef opcode_table_byte, $80
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
.endr*/


.SECTION "dispatch OPCODE write", BANK OPCODEBANK BASE $80 FREE 
; long jump to this to atart executing opcodes!
StartDispatchOpcode:
    DispatchOpcode
.ENDS

;--------------------------------------
; Begin opcode definitions
;--------------------------------------

;--------------------------------------
; 8-bit loads
;--------------------------------------

;--------------------------------------
; ld r, r

.macro ld_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_R\@:
    ldy <reg
    DispatchOpcode
.ENDS
.endm

ld_a_r $78, GB_BC + 1
ld_a_r $79, GB_BC
ld_a_r $7A, GB_DE + 1
ld_a_r $7B, GB_DE
ld_a_r $7C, GB_HL + 1
ld_a_r $7D, GB_HL

.macro ld_r_a ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld r, a\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_R_A\@:
    sty <reg
    DispatchOpcode
.ENDS
.endm

ld_r_a $47, GB_BC + 1
ld_r_a $4F, GB_BC
ld_r_a $57, GB_DE + 1
ld_r_a $5F, GB_DE
ld_r_a $67, GB_HL + 1
ld_r_a $6F, GB_HL

.macro ld_r_r ARGS opcode, dest, src
GetOpcodeTableAddress opcode
.SECTION "ld r, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_R_R\@:
    ldx <src
    stx <dest
    DispatchOpcode
.ENDS
.endm

ld_r_r $40, GB_BC + 1, GB_BC + 1
ld_r_r $41, GB_BC + 1, GB_BC
ld_r_r $42, GB_BC + 1, GB_DE + 1
ld_r_r $43, GB_BC + 1, GB_DE
ld_r_r $44, GB_BC + 1, GB_HL + 1
ld_r_r $45, GB_BC + 1, GB_HL

ld_r_r $48, GB_BC, GB_BC + 1
ld_r_r $49, GB_BC, GB_BC
ld_r_r $4A, GB_BC, GB_DE + 1
ld_r_r $4B, GB_BC, GB_DE
ld_r_r $4C, GB_BC, GB_HL + 1
ld_r_r $4D, GB_BC, GB_HL

ld_r_r $50, GB_DE + 1, GB_BC + 1
ld_r_r $51, GB_DE + 1, GB_BC
ld_r_r $52, GB_DE + 1, GB_DE + 1
ld_r_r $53, GB_DE + 1, GB_DE
ld_r_r $54, GB_DE + 1, GB_HL + 1
ld_r_r $55, GB_DE + 1, GB_HL

ld_r_r $58, GB_DE, GB_BC + 1
ld_r_r $59, GB_DE, GB_BC
ld_r_r $5A, GB_DE, GB_DE + 1
ld_r_r $5B, GB_DE, GB_DE
ld_r_r $5C, GB_DE, GB_HL + 1
ld_r_r $5D, GB_DE, GB_HL

ld_r_r $60, GB_HL + 1, GB_BC + 1
ld_r_r $61, GB_HL + 1, GB_BC
ld_r_r $62, GB_HL + 1, GB_DE + 1
ld_r_r $63, GB_HL + 1, GB_DE
ld_r_r $64, GB_HL + 1, GB_HL + 1
ld_r_r $65, GB_HL + 1, GB_HL

ld_r_r $68, GB_HL, GB_BC + 1
ld_r_r $69, GB_HL, GB_BC
ld_r_r $6A, GB_HL, GB_DE + 1
ld_r_r $6B, GB_HL, GB_DE
ld_r_r $6C, GB_HL, GB_HL + 1
ld_r_r $6D, GB_HL, GB_HL

;--------------------------------------
; ld r, n

GetOpcodeTableAddress $3E
.SECTION "ld a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_d8:
    ply
    DispatchOpcode
.ENDS

.macro ld_r_d8 ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld r, d8\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_R_d8\@:
    plx
    stx <reg
    DispatchOpcode
.ENDS
.endm

ld_r_d8 $06, GB_BC + 1
ld_r_d8 $0E, GB_BC
ld_r_d8 $16, GB_DE + 1
ld_r_d8 $1E, GB_DE
ld_r_d8 $26, GB_HL + 1
ld_r_d8 $2E, GB_HL

;--------------------------------------
; ld r, [hl]

.macro ld_a_rr ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld a, [rr]\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_RR\@:
    ; I would seta8 here but it's actually faster to just not care, and the high byte gets discarded by tay
    lda (<reg)
    /*.db $B2 ; lda (dp)
    .db <reg ; gets around a stupid WLA-DX issue*/
    tay
    DispatchOpcode
.ENDS
.endm

ld_a_rr $0A, GB_BC
ld_a_rr $1A, GB_DE
ld_a_rr $7E, GB_HL

.macro ld_rr_a ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld [rr], a\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_RR_A\@:
    seta8
    tya
    .db $92 ; sta (dp)
    .db <reg ; gets around a stupid WLA-DX issue
    DispatchOpcode
.ENDS
.endm

ld_rr_a $02, GB_BC
ld_rr_a $12, GB_DE
ld_rr_a $77, GB_HL

.macro ld_r_hl ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld r, [hl]\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_R_HL\@:
    seta8
    .db $B2 ; lda (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <reg
    DispatchOpcode
.ENDS
.endm

ld_r_hl $46, GB_BC + 1
ld_r_hl $4E, GB_BC
ld_r_hl $56, GB_DE + 1
ld_r_hl $5E, GB_DE
ld_r_hl $66, GB_HL + 1
ld_r_hl $6E, GB_HL

.macro ld_hl_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "ld [hl], r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL_R\@:
    seta8
    lda <reg
    .db $92 ; sta (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    DispatchOpcode
.ENDS
.endm

ld_hl_r $70, GB_BC + 1
ld_hl_r $71, GB_BC
ld_hl_r $72, GB_DE + 1
ld_hl_r $73, GB_DE
ld_hl_r $74, GB_HL + 1
ld_hl_r $75, GB_HL

GetOpcodeTableAddress $36
.SECTION "ld [hl], d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL_d8:
    seta8
    pla
    .db $92 ; sta (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $EA
.SECTION "ld [a16], a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_D16_A:
    seta8
    setxy16
    tya
    plx
    sta GB_MEMORY,x
    setxy8
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $FA
.SECTION "ld a, [a16]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_D16:
    setxy16
    plx
    ldy GB_MEMORY,x ; this loads a second byte, but that gets discarded nest
    setxy8 ; this saves time overall
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $E2
.SECTION "ld (c), a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_C_A: ; NOT IMPLEMENTED
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $F2
.SECTION "ld a, (c)", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_C: ; NOT IMPLEMENTED
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $E0
.SECTION "ldh (a8), a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LDH_A8_A: ; NOT IMPLEMENTED
    plx
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $F0
.SECTION "ldh a, (a8)", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LDH_A_A8: ; NOT IMPLEMENTED
    plx
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $32
.SECTION "ld (hl-), a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL_DEC_A: ; We'll assume this isn't used to access IO
    seta8
    tya
    .db $92 ; sta (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue    
    seta16
    dec <GB_HL
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $22
.SECTION "ld (hl+), a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_HL_INC_A: ; We'll assume this isn't used to access IO
    seta8
    tya
    .db $92 ; sta (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue    
    seta16
    inc <GB_HL
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $3A
.SECTION "ld a, (hl-)", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_HL_DEC: ; We'll assume this isn't used to access IO
    .db $B2 ; lda (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    tay ; this is a 16-bit load, but that shouldn't cause problems
    seta16
    dec <GB_HL
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $2A
.SECTION "ld a, (hl+)", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_A_HL_INC: ; We'll assume this isn't used to access IO
    .db $B2 ; lda (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    tay ; this is a 16-bit load, but that shouldn't cause problems
    seta16
    inc <GB_HL
    DispatchOpcode
.ENDS

;--------------------------------------
; 16-bit loads
;--------------------------------------

.macro ld_rr_d16 ARGS opcode, regpair
GetOpcodeTableAddress opcode
.SECTION "ld rr, d16\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_RR_d16\@:
    seta16
    pla
    sta <regpair
    DispatchOpcode
.ENDS
.endm

ld_rr_d16 $01, GB_BC
ld_rr_d16 $11, GB_DE
ld_rr_d16 $21, GB_HL
ld_rr_d16 $31, GB_SP

GetOpcodeTableAddress $08
.SECTION "ld (a16), sp", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_a16_SP:
    setaxy16
    plx
    lda <GB_SP
    sta GB_MEMORY,x
    setxy8
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $F9
.SECTION "ld sp, hl", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
LD_SP_HL:
    seta16
    lda <GB_HL
    sta <GB_SP
    DispatchOpcode
.ENDS

;--------------------------------------
; stack ops

GetOpcodeTableAddress $F5
.SECTION "push af", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
PUSH_AF:
    ; NOT IMPLEMENTED
    DispatchOpcode
.ENDS

.macro push_rr ARGS opcode, regpair
GetOpcodeTableAddress opcode
.SECTION "push rr\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
PUSH_RR\@:
    setaxy16 
    ldx <GB_SP
    dex 
    dex 
    lda <regpair 
    sta GB_MEMORY,x  
    stx <GB_SP 
    setxy8 
    DispatchOpcode
.ENDS
.endm

push_rr $C5, GB_BC
push_rr $D5, GB_DE
push_rr $E5, GB_HL

GetOpcodeTableAddress $F1
.SECTION "pop af", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
POP_AF:
    ; NOT IMPLEMENTED
    DispatchOpcode
.ENDS


.macro pop_rr ARGS opcode, regpair
GetOpcodeTableAddress opcode
.SECTION "pop rr\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
POP_RR\@:
    setaxy16 
    ldx <GB_SP
    lda GB_MEMORY,x
    sta <regpair
    inx
    inx
    stx <GB_SP
    setxy8
    DispatchOpcode
.ENDS
.endm

pop_rr $C1, GB_BC
pop_rr $D1, GB_DE
pop_rr $E1, GB_HL


;--------------------------------------
; 8-bit arithmetic/logic
;--------------------------------------

.macro add_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "add a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADD_A_R\@:
    seta8
    clc
    tya
    adc <reg
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    tay
    DispatchOpcode
.ENDS
.endm

add_a_r $80, GB_BC + 1
add_a_r $81, GB_BC
add_a_r $82, GB_DE + 1
add_a_r $83, GB_DE
add_a_r $84, GB_HL + 1
add_a_r $85, GB_HL

GetOpcodeTableAddress $86
.SECTION "add a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADD_A_HL:
    seta8
    clc
    tya
    .db $72 ; adc (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    tay
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $87
.SECTION "add a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADD_A_A:
    seta8
    tya
    asl a
    ror <GB_CARRYFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $C6
.SECTION "add a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADD_A_d8:
    seta8
    clc
    tya
    adc 1,s ; add the top of the stack
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    plx ; discard the d8
    DispatchOpcode
.ENDS

.macro adc_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "adc a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADC_A_R\@:
    seta8
    tya
    rol <GB_CARRYFLAG
    adc <reg
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    tay
    DispatchOpcode
.ENDS
.endm

adc_a_r $88, GB_BC + 1
adc_a_r $89, GB_BC
adc_a_r $8A, GB_DE + 1
adc_a_r $8B, GB_DE
adc_a_r $8C, GB_HL + 1
adc_a_r $8D, GB_HL

GetOpcodeTableAddress $8E
.SECTION "adc a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADC_A_HL:
    seta8
    tya
    rol <GB_CARRYFLAG
    .db $72 ; adc (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    tay
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $8F
.SECTION "adc a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADC_A_A:
    seta8
    tya
    rol <GB_CARRYFLAG
    rol a
    ror <GB_CARRYFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $CE
.SECTION "adc a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
ADC_A_d8:
    seta8
    tya
    rol <GB_CARRYFLAG
    adc 1,s ; add the top of the stack
    sta <GB_ZEROFLAG
    ror <GB_CARRYFLAG
    plx ; discard the d8
    DispatchOpcode
.ENDS

.macro sub_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "sub a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SUB_A_R\@:
    seta8
    sec
    tya
    sbc <reg
    sta <GB_ZEROFLAG
    tay
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode

@setCarry
    ; all sub opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS
.endm

sub_a_r $90, GB_BC + 1
sub_a_r $91, GB_BC
sub_a_r $92, GB_DE + 1
sub_a_r $93, GB_DE
sub_a_r $94, GB_HL + 1
sub_a_r $95, GB_HL

GetOpcodeTableAddress $96
.SECTION "sub a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SUB_A_HL:
    seta8
    sec
    tya
    .db $F2 ; sbc (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    tay
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all sub opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $97
.SECTION "sub a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SUB_A_A:
    ; zeros the accumulator
    ldy #0
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $D6
.SECTION "sub a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SUB_A_D8:
    seta8
    sec
    tya
    sbc 1,s ; add the top of the stack
    sta <GB_ZEROFLAG
    tay
    plx ; discard the d8
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all sub opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

.macro sbc_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "sbc a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SBC_A_R\@:
    ; invert and fetch the carry flag
    seta8
    lda <GB_CARRYFLAG
    eor #$FF
    rol a
    sbc <reg
    sta <GB_ZEROFLAG
    tay
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all sbc opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS
.endm

sbc_a_r $98, GB_BC + 1
sbc_a_r $99, GB_BC
sbc_a_r $9A, GB_DE + 1
sbc_a_r $9B, GB_DE
sbc_a_r $9C, GB_HL + 1
sbc_a_r $9D, GB_HL

GetOpcodeTableAddress $9E
.SECTION "sbc a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SBC_A_HL:
    ; invert and fetch the carry flag
    seta8
    lda <GB_CARRYFLAG
    eor #$FF
    rol a
    .db $F2 ; sbc (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    tay
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all sbc opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $9F
.SECTION "sbc a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SBC_A_A:
    ; sets all bits of a to carry flag, preserves carry flag
    ldx <GB_CARRYFLAG
    bmi @carrySet
@carryClear
    ldy #0
    DispatchOpcode
@carrySet
    ldy #$FF
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $DE
.SECTION "sbc a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
SBC_A_D8:
    ; invert and fetch the carry flag
    seta8
    lda <GB_CARRYFLAG
    eor #$FF
    rol a
    sbc 1,s ; sbc the top of the stack
    sta <GB_ZEROFLAG
    tay
    plx ; discard the d8
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all sbc opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

.macro and_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "and a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
AND_A_R\@:
    seta8
    tya
    and <reg
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS
.endm

and_a_r $A0, GB_BC + 1
and_a_r $A1, GB_BC
and_a_r $A2, GB_DE + 1
and_a_r $A3, GB_DE
and_a_r $A4, GB_HL + 1
and_a_r $A5, GB_HL

GetOpcodeTableAddress $A6
.SECTION "and a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
AND_A_HL:
    seta8
    tya
    .db $32 ; and (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $A7
.SECTION "and a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
AND_A_A:
    ; all this does is set the zero flag
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $E6
.SECTION "and a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
AND_A_D8:
    seta8
    tya
    and 1,s ; and the top of the stack
    sta <GB_ZEROFLAG
    tay
    plx ; discard the d8
    DispatchOpcode
.ENDS

.macro xor_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "xor a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
XOR_A_R\@:
    seta8
    tya
    eor <reg
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS
.endm

xor_a_r $A8, GB_BC + 1
xor_a_r $A9, GB_BC
xor_a_r $AA, GB_DE + 1
xor_a_r $AB, GB_DE
xor_a_r $AC, GB_HL + 1
xor_a_r $AD, GB_HL

GetOpcodeTableAddress $AE
.SECTION "xor a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
XOR_A_HL:
    seta8
    tya
    .db $52 ; eor (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS
  
GetOpcodeTableAddress $af
.SECTION "xor a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
XOR_A:
    ldy #0
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $EE
.SECTION "xor a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
XOR_A_D8:
    seta8
    tya
    eor 1,s ; eor the top of the stack
    sta <GB_ZEROFLAG
    tay
    plx ; discard the d8
    DispatchOpcode
.ENDS

.macro or_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "or a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
OR_A_R\@:
    seta8
    tya
    ora <reg
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS
.endm

or_a_r $B0, GB_BC + 1
or_a_r $B1, GB_BC
or_a_r $B2, GB_DE + 1
or_a_r $B3, GB_DE
or_a_r $B4, GB_HL + 1
or_a_r $B5, GB_HL

GetOpcodeTableAddress $B6
.SECTION "or a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
OR_A_HL:
    seta8
    tya
    .db $12 ; ora (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    tay
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $B7
.SECTION "or a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
OR_A_A:
    ; all this does is set the zero flag
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $F6
.SECTION "or a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
OR_A_D8:
    seta8
    tya
    ora 1,s ; ora the top of the stack
    sta <GB_ZEROFLAG
    tay
    plx ; discard the d8
    DispatchOpcode
.ENDS

.macro cp_a_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "cp a, r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CP_A_R\@:
    seta8
    sec
    tya
    sbc <reg
    sta <GB_ZEROFLAG
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all cp opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS
.endm

cp_a_r $B8, GB_BC + 1
cp_a_r $B9, GB_BC
cp_a_r $BA, GB_DE + 1
cp_a_r $BB, GB_DE
cp_a_r $BC, GB_HL + 1
cp_a_r $BD, GB_HL

GetOpcodeTableAddress $BE
.SECTION "cp a, [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CP_A_HL:
    seta8
    sec
    tya
    .db $F2 ; sbc (dp)
    .db <GB_HL ; gets around a stupid WLA-DX issue
    sta <GB_ZEROFLAG
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all cp opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $BF
.SECTION "cp a, a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CP_A_A:
    ; all this does is set the zero flag
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $FE
.SECTION "cp a, d8", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CP_A_D8:
    seta8
    sec
    tya
    sbc 1,s ; sbc the top of the stack
    sta <GB_ZEROFLAG
    plx ; discard the d8
    ; now we invert the carry flag before storing it
    bcc @setCarry
@clearCarry
    stz <GB_CARRYFLAG
    DispatchOpcode
@setCarry
    ; all cp opcodes have bit 7 set, so we can store it as carry
    stx <GB_CARRYFLAG
    DispatchOpcode
.ENDS

;--------------------------------------
; 8-bit inc/dec

.macro inc_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "inc r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
INC_R\@:
    seta8
    inc <reg
    beq @setZero
@clearZero
    stx <GB_ZEROFLAG
    DispatchOpcode
@setZero
    stz <GB_ZEROFLAG
    DispatchOpcode
.ENDS
.endm

inc_r $04, GB_BC + 1
inc_r $0C, GB_BC
inc_r $14, GB_DE + 1
inc_r $1C, GB_DE
inc_r $24, GB_HL + 1
inc_r $2C, GB_HL

GetOpcodeTableAddress $34
.SECTION "inc [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
INC_HL:
    seta8
    setxy16
    ldx <GB_HL
    inc GB_MEMORY,x
    setxy8
    beq @setZero   
@clearZero
    stx <GB_ZEROFLAG
    DispatchOpcode
@setZero
    stz <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $3C
.SECTION "inc a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
INC_A:
    iny
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

.macro dec_r ARGS opcode, reg
GetOpcodeTableAddress opcode
.SECTION "dec r\@", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
DEC_R\@:
    seta8
    dec <reg
    beq @setZero
@clearZero
    stx <GB_ZEROFLAG
    DispatchOpcode
@setZero
    stz <GB_ZEROFLAG
    DispatchOpcode
.ENDS
.endm

dec_r $05, GB_BC + 1
dec_r $0D, GB_BC
dec_r $15, GB_DE + 1
dec_r $1D, GB_DE
dec_r $25, GB_HL + 1
dec_r $2D, GB_HL

GetOpcodeTableAddress $35
.SECTION "dec [hl]", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
DEC_HL:
    seta8
    setxy16
    ldx <GB_HL
    dec GB_MEMORY,x
    setxy8
    beq @setZero
@clearZero
    stx <GB_ZEROFLAG
    DispatchOpcode
@setZero
    stz <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $3D
.SECTION "dec a", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
DEC_A:
    dey
    sty <GB_ZEROFLAG
    DispatchOpcode
.ENDS

GetOpcodeTableAddress $CB
.SECTION "CB prefix", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CB_PREFIX:
    plx
    jmp.l StartDispatchPrefix
.ENDS

;--------------------------------------
; Control flow
;--------------------------------------

.macro jr ARGS opcode, condition
GetOpcodeTableAddress opcode
.SECTION "jr {condition}", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
JR_{condition}:
    .IF condition == "nonzero"
        ldx <GB_ZEROFLAG
        beq @noJump
    .ELIF condition == "zero"
        ldx <GB_ZEROFLAG
        bne @noJump
    .ELIF condition == "noncarry"
        ldx <GB_CARRYFLAG
        bmi @noJump
    .ELIF condition == "carry"
        ldx <GB_CARRYFLAG
        bpl @noJump
    .ELIF condition == "always"
        ; fall through
    .ELSE
        .ERROR "Invalid condition for jr"
    .ENDIF

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
.endm

jr $20, "nonzero"
jr $28, "zero"
jr $30, "noncarry"
jr $38, "carry"
jr $18, "always"

.macro jp ARGS opcode, condition
GetOpcodeTableAddress opcode
.SECTION "jp {condition}", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
JP_{condition}:
    .IF condition == "nonzero"
        ldx <GB_ZEROFLAG
        beq @noJump
    .ELIF condition == "zero"
        ldx <GB_ZEROFLAG
        bne @noJump
    .ELIF condition == "noncarry"
        ldx <GB_CARRYFLAG
        bmi @noJump
    .ELIF condition == "carry"
        ldx <GB_CARRYFLAG
        bpl @noJump
    .ELIF condition == "always"
        ; fall through
    .ELSE
        .ERROR "Invalid condition for jp"
    .ENDIF

@jump
    seta16
    pla
    dec a
    ora #$8000 ; read from the upper half
    tas
    DispatchOpcode

@noJump
    plx
    plx ; discard the address
    DispatchOpcode
.ENDS
.endm

jp $C2, "nonzero"
jp $CA, "zero"
jp $D2, "noncarry"
jp $DA, "carry"
jp $C3, "always"

.macro call ARGS opcode, condition
GetOpcodeTableAddress opcode
.SECTION "call {condition}", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
CALL_{condition}:
    .IF condition == "nonzero"
        ldx <GB_ZEROFLAG
        beq @noJump
    .ELIF condition == "zero"
        ldx <GB_ZEROFLAG
        bne @noJump
    .ELIF condition == "noncarry"
        ldx <GB_CARRYFLAG
        bmi @noJump
    .ELIF condition == "carry"
        ldx <GB_CARRYFLAG
        bpl @noJump
    .ELIF condition == "always"
        ; fall through
    .ELSE
        .ERROR "Invalid condition for call"
    .ENDIF

@jump
    setaxy16
    ; push the return address
    tsa
    inc a
    inc a ; step past the destination address
    ldx <GB_SP
    dex
    dex
    sta GB_MEMORY,x
    stx <GB_SP
    pla ; fetch the destination address
    dec a ; step back since 65xx increments at the start of pull
    ora #$8000 ; read from the upper half
    tas
    setxy8
    DispatchOpcode

@noJump
    plx ; discard the destination address
    plx
    DispatchOpcode
.ENDS
.endm

call $C4, "nonzero"
call $CC, "zero"
call $D4, "noncarry"
call $DC, "carry"
call $CD, "always"

.macro ret ARGS opcode, condition
GetOpcodeTableAddress opcode
.SECTION "ret {condition}", BANK OPCODEBANK BASE $80 ORGA opcode_table_address FORCE
RET_{condition}:
    .IF condition == "nonzero"
        ldx <GB_ZEROFLAG
        beq @noJump
    .ELIF condition == "zero"
        ldx <GB_ZEROFLAG
        bne @noJump
    .ELIF condition == "noncarry"
        ldx <GB_CARRYFLAG
        bmi @noJump
    .ELIF condition == "carry"
        ldx <GB_CARRYFLAG
        bpl @noJump
    .ELIF condition == "always"
        ; fall through
    .ELSE
        .ERROR "Invalid condition for ret"
    .ENDIF
@jump
    setxy16
    ldx <GB_SP
    lda GB_MEMORY,x
    inx
    inx
    stx <GB_SP
    tas
    DispatchOpcode

@noJump
    DispatchOpcode
.ENDS
.endm

ret $C0, "nonzero"
ret $C8, "zero"
ret $D0, "noncarry"
ret $D8, "carry"
ret $C9, "always"














    