SECTION "DIV2 WRAM", WRAM0, ALIGN[3]
_MD16temp:  ds 2
_MD16count: db

SECTION "UXN Instructions", ROM0

; UXN Instruction Implementations

; Support macros
WST_HL: MACRO ; instructions which add to the stack start at ptr
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    ld      l, a
    ENDM

WST_HL_dec: MACRO ; instructions which consume the stack start at ptr-1
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    ld      l, a
    dec     l
    ENDM

WST_PTR_L: MACRO
    ld      a, l
    ldh     [wst_ptr], a
    ENDM

WST_HA_dec_ptr: MACRO
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ldh     [wst_ptr], a
    ENDM

RST_HL: MACRO
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    ld      l, a
    ENDM

RST_HL_dec: MACRO ; instructions which consume the stack start at ptr-1
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    ld      l, a
    dec     l
    ENDM

RST_PTR_L: MACRO
    ld      a, l
    ldh     [rst_ptr], a
    ENDM

RST_HA_dec_ptr: MACRO
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ldh     [rst_ptr], a
    ENDM

PC_to_HL: MACRO
    ldh     a, [pc]
    ld      h, a
    ldh     a, [pc+1]
    ld      l, a
    ENDM

HL_to_PC: MACRO
    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a
    ENDM

PC_to_B: MACRO
    PC_to_HL
    ld      b, [hl]     ; literal value
    ENDM

; Convert BC in UXN space to GB memory space, accounting for banks
; Note: BC is in UXN memory space ($0000-$ffff), and needs to be converted to our version of that ($a000-$bfff, banked)
; TODO: Optimize this, and account for banks
BC_to_UXN_Banked: MACRO
    push    hl
    ld      hl, $A000
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl
    ENDM


instr_NIL::
instr_BRK::
    ret

; INC a -- b
instr_INC::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
.continue
    ld      l, a
    dec     l
    inc     [hl]
    ret

; POP a -- 
instr_POP::
    ldh     a, [wst_ptr]
    dec     a
    ldh     [wst_ptr], a
    ret

; NIP a b -- b
instr_NIP::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      l, a
    ld      a, [hld]
    ld      [hli], a
    ret

; SWP a b -- b a
instr_SWP::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
.continue
    ld      l, a
    dec     l
    ld      a, [hld]
    ld      b, [hl]
    ld      [hli], a
    ld      [hl], b
    ret
    
; ROT a b c -- b c a
instr_ROT::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
.continue
    ld      l, a
    dec     l
    ld      b, [hl]
    dec     l
    ld      a, [hld]
    ld      c, [hl]
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hl], c
    ret

; DUP a -- a a
instr_DUP::
    WST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    WST_PTR_L
    ret

; OVR a b -- a b a
instr_OVR::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; EQU a b -- bool8
instr_EQU::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nz, .notEqual
    inc     a
.notEqual
    ld      [hli], a
    WST_PTR_L
    ret

; NEQ a b -- bool8
instr_NEQ::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      z, .equal
    inc     a
.equal
    ld      [hli], a
    WST_PTR_L
    ret

; GTH a b -- bool8
instr_GTH::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nc, .notGreater
    inc     a
.notGreater
    ld      [hli], a
    WST_PTR_L
    ret

; LTH a b -- bool8
instr_LTH::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      c, .notLesser
    jr      z, .notLesser
    inc     a
.notLesser
    ld      [hli], a
    WST_PTR_L
    ret

; JMP addr --
instr_JMP::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      l, a
    ld      c, [hl]
.jump
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    PC_to_HL
    add     hl, bc
    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a
    ret

; JCN cond8 addr --
instr_JCN::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      l, a
    ld      c, [hl]
    ld      a, c
    or      a
    ret     z   ; condition not met
    inc     l
    ld      c, [hl]
    jr      instr_JMP.jump

; JSR addr --
instr_JSR::
    ldh     a, [pc]
    ld      b, a
    ldh     a, [pc+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -uxn_memory
    add     hl, bc
    ld      b, h
    ld      c, l

    RST_HL
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    ; UXN return address now on RST, continue with normal JMP
    jr      instr_JMP

; STH a --
; TODO: Check if removing macros opens up optimizations
instr_STH::
    WST_HL_dec
    ld      b, [hl]
    WST_PTR_L   ; destroys A
    RST_HL      ; destroys A
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; LDZ addr8 -- value
instr_LDZ::
    WST_HL_dec
.continue
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hl], a
    ret

; STZ value addr8 --
instr_STZ::
    WST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    dec     l
    ld      a, [hl]
    ld      [bc], a
    WST_PTR_L
    ret

; LDR addr8 -- value
instr_LDR::
    WST_HL_dec
.continue
    ld      c, [hl]
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    push    hl
    PC_to_HL
    add     hl, bc
    ld      a, [hl]
    pop     hl
    ld      [hl], a
    ret

; STR value addr8 --
instr_STR::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
    ld      d, [hl]
.continue
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    push    hl
    PC_to_HL
    add     hl, bc
    ld      [hl], d
    pop     hl
    ret

; LDA addr16 -- value
instr_LDA::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    BC_to_UXN_Banked
    ld      a, [bc]
    ld      [hl], a
    ret

; STA value addr16 --
instr_STA::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      d, [hl]
    WST_PTR_L
.continue
    BC_to_UXN_Banked
    ld      a, d
    ld      [bc], a
    ret

instr_DEI::
    ret

; DEO val device8 --
instr_DEO::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]

    ; DEVPOKE8
    ld      hl, devices
    ld      a, d
    add     l
    ld      l, a

    ld      [hl], b

    ; get handler address
    ld      a, d
    and     $F0
    srl     a
    add     4       ; DEO handler offset
    ld      hl, device_handlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; ADD a b -- c
instr_ADD::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    ld      [hli], a
    WST_PTR_L
    ret

; SUB a b -- c
instr_SUB::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    dec     l
    ld      [hli], a
    WST_PTR_L
    ret

; MUL a b -- c
instr_MUL::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    ld      e, [hl]
    push    hl
    ld      h, a
    ld      l, 0
    ld      d, l

    ; Taken from: https://www.cpcwiki.eu/index.php/Programming:Integer_Multiplication#Classic_8bit_.2A_8bit_Unsigned
    ; TODO: Compare to https://github.com/pinobatch/little-things-gb/blob/f0d7ae77e6b6beebfd4a740f5f8f0ace5e330a11/bdos/src/math.z80#L227
    sla     h
    jr      nc, :+
    ld      l, e
:
    REPT 7
    add     hl, hl
    jr      nc, :+
    add     hl, de
:   
    ENDR

    ld      a, l
    pop     hl
    ld      [hl], a
    ret

; DIV a b -- c
instr_DIV::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      c, [hl]
    dec     l
    ld      e, [hl]

    ; Source: http://map.grauw.nl/articles/mult_div_shifts.php
    xor     a
    ld      b, 8
.loop
    rl      e
    rla
    sub     c
    jr      nc, .noAdd
    add     a, c
.noAdd
    dec     b
    jr      nz, .loop
    ld      b, a
    ld      a, e
    rla
    cpl

    ld      [hl], a
    ret

; AND a b -- c
instr_AND::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    and     [hl]
    ld      [hl], a
    ret

; ORA a b -- c
instr_ORA::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    or      [hl]
    ld      [hl], a
    ret

; EOR a b -- c
instr_EOR::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    xor     [hl]
    ld      [hl], a
    ret

; SFT a shift8 -- c
instr_SFT::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    ld      a, b
    and     $0F     ; get right shift count
    or      a
    jr      z, .doneRightShift
.rightShift
    srl     c
    dec     a
    jr      nz, .rightShift
.doneRightShift
    ld      a, b
    and     $F0     ; get left shift count
    swap    a
    or      a
    jr      z, .doneLeftShift
.leftShift
    sla     c
    dec     a
    jr      nz, .leftShift
.doneLeftShift
    ld      [hl], c
    ret

; INC2 a -- b
instr_INC2::
    WST_HL_dec
.continue
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    inc     bc
    ld      [hl], b
    inc     l
    ld      [hl], c
    ret

; POP2 a b -- 
instr_POP2::
    ldh     a, [wst_ptr]
    dec     a
    dec     a
    ldh     [wst_ptr], a
    ret

; DUP2 a b -- a b a b
instr_DUP2::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; NIP2 a b c d -- c d
instr_NIP2::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    WST_PTR_L
.continue
    dec     l
    ld      [hl], b
    dec     l
    ld      [hl], c
    ret

; SWP2 a b c d -- c d a b
instr_SWP2::
    WST_HL_dec
.continue
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    ld      [hl], e
    inc     l
    ld      [hl], d
    ret

; OVR2 a b c d -- a b c d a b
instr_OVR2::
    WST_HL_dec
    dec     l
    dec     l
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; ROT2 a b c d e f -- c d e f a b
instr_ROT2::
    WST_HL_dec
.continue
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    ld      [hl], b
    dec     l
    ld      a, [hl] ; use A so we can hli later
    ld      [hl], c
    dec     l
    ld      b, [hl]
    ld      [hl], d
    dec     l
    ld      c, [hl]
    ld      [hli], a
    inc     l
    inc     l
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], b
    ret

; EQU2 a b c d -- bool8
instr_EQU2::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      nz, .notEqual
    ld      a, c
    cp      e
    jr      nz, .notEqual
    inc     h
.notEqual
    ld      a, h
    pop     hl
    ld      [hli], a
    WST_PTR_L
    ret

; NEQ2 a b c d -- bool8
instr_NEQ2::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      z, .equal
    ld      a, c
    cp      e
    jr      z, .equal
    inc     h
.equal
    ld      a, h
    pop     hl
    ld      [hli], a
    WST_PTR_L
    ret

; GTH2 a b c d -- bool8
instr_GTH2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      c, .greaterThan     ; if d > b, de > bc
    jr      nz, .notGreaterThan      
    ld      a, c
    cp      e
    jr      nc, .notGreaterThan
.greaterThan
    inc     h
.notGreaterThan
    ld      a, h
    pop     hl
    ld      [hli], a
    WST_PTR_L
    ret

; LTH2 a b d c -- bool8
instr_LTH2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl
    ld      h, 0
    ld      a, d
    cp      b
    jr      c, .lessThan
    jr      nz, .notLessThan      
    ld      a, e
    cp      c
    jr      nc, .notLessThan
.lessThan
    inc     h
.notLessThan
    ld      a, h
    pop     hl
    ld      [hli], a
    WST_PTR_L
    ret

; JMP2 addr --
instr_JMP2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]
.jump
    ld      hl, uxn_memory
    add     hl, bc
    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a
    ret

; JCN2 cond addr --
instr_JCN2::
    WST_HL_dec
    dec     l
    dec     l
    ld      c, [hl]
    WST_PTR_L
    ld      a, c
    or      a
    ret     z   ; condition not met
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    jr      instr_JMP2.jump

; JSR2 addr --
instr_JSR2::
    RST_HL
    ldh     a, [pc]
    ld      b, a
    ldh     a, [pc+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -uxn_memory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    jr      instr_JMP2

; STH2 a b --
instr_STH2::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    WST_PTR_L   ; destroys A
    RST_HL      ; destroys A
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; LDZ addr8 -- value
instr_LDZ2::
    WST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    WST_PTR_L
    ret

; STZ value addr8 --
instr_STZ2::
    WST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    inc     c
    dec     l
    ld      a, [hld]
    ld      [bc], a
    dec     c
    ld      a, [hl]
    ld      [bc], a
    WST_PTR_L
    ret

; LDR2 addr8 -- value
instr_LDR2::
    WST_HL_dec
    ld      c, [hl]
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    push    hl
    PC_to_HL
    add     hl, bc
    ld      a, [hli]    ; TODO: Deal with SRAM banks
    ld      b, [hl]
    pop     hl
    ld      [hli], a
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; STR2 value addr8 --
instr_STR2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    WST_PTR_L
.continue
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    push    hl
    PC_to_HL
    add     hl, bc
    ld      [hl], d ; TODO: Deal with SRAM banks!
    inc     hl
    ld      [hl], e
    pop     hl
    ret

; LDA2 addr16 -- value
instr_LDA2::
    WST_HL_dec
.continue
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    BC_to_UXN_Banked
    ld      a, [bc]
    ld      [hli], a
    inc     bc      ; TODO: Handle bank wrapping
    ld      a, [bc]
    ld      [hl], a
    ret

; STA2 value addr16 --
instr_STA2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    WST_PTR_L
.continue
    BC_to_UXN_Banked
    ld      a, d
    ld      [bc], a
    inc     bc      ; TODO: Handle bank wrapping
    ld      a, e
    ld      [bc], a
    ret

instr_DEI2::
    rst     Crash


; DEO val device8 --
instr_DEO2::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    WST_PTR_L
.continue
    ; DEVPOKE16
    ld      hl, devices
    ld      a, d
    add     l
    ld      l, a

    ld      [hl], b
    inc     l
    ld      [hl], c

    ; get handler address
    ld      a, d
    and     $F0
    srl     a
    add     6       ; DEO2 handler offset
    ld      hl, device_handlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; ADD2 a b -- c
instr_ADD2::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl
    ld      h, d
    ld      l, e
    add     hl, bc
    ld      a, h
    ld      e, l
    pop     hl
    ld      [hli], a
    ld      [hl], e
    ret

; SUB2 a b -- c
instr_SUB2::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    ld      a, c
    sub     e
    ld      c, a
    ld      a, b
    sbc     d
    ld      [hli], a
    ld      [hl], c
    ret


; MUL2 a b c d -- e f
instr_MUL2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl

    ; http://map.grauw.nl/articles/mult_div_shifts.php

    ld      a, b
    ld      b, 16
.loop
    add     hl, hl
    sla     c
    rla
    jr      nc, .noAdd
    add     hl,de
.noAdd
    dec     b
    jr      nz, .loop
    ld      b, h
    ld      a, l

    pop     hl
    ld      [hl], b
    inc     l
    ld      [hl], a
    ret

; DIV2 a b c d -- e f
instr_DIV2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl

    ; http://www.devrs.com/gb/asmcode.php (U161616a v1.0 by Jeff Frohwein)
    ld      hl, _MD16temp
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    ld      [hl], 17
    ld      bc, 0
.nextBit:
    ld      l, LOW(_MD16count)
    ld      a, e
    rla
    ld      e, a
    ld      a, d
    rla
    ld      d, a
    dec     [hl]
    jr      z, .done
    ld      a, c
    rla
    ld      c, a
    ld      a, b
    rla
    ld      b, a
    dec     l
    dec     l
    ld      a, c
    sub     [hl]
    ld      c, a
    inc     l
    ld      a, b
    sbc     a, [hl]
    ld      b, a
    jr      nc, .noAdd

    dec     hl
    ld      a, c
    add     a, [hl]
    ld      c, a
    inc     l
    ld      a, b
    adc     a, [hl]
    ld      b, a
.noAdd:
    ccf
    jr      .nextBit
.done

    pop     hl
    ld      [hl], d
    inc     l
    ld      [hl], e
    ret

; AND2 a b -- c
instr_AND2::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      a, [hld]
    ld      b, [hl]
    dec     l
    and     [hl]
    ld      [hld], a
    ld      a, b
    and     [hl]
    ld      [hl], a
    ret

; ORA2 a b -- c
instr_ORA2::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      a, [hld]
    ld      b, [hl]
    dec     l
    or      [hl]
    ld      [hld], a
    ld      a, b
    or      [hl]
    ld      [hl], a
    ret

; EOR2 a b -- c
instr_EOR2::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [wst_ptr], a
.continue
    ld      a, [hld]
    ld      b, [hl]
    dec     l
    xor     [hl]
    ld      [hld], a
    ld      a, b
    xor     [hl]
    ld      [hl], a
    ret

; SFT2 a shift8 -- c
instr_SFT2::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      d, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    ld      a, d
    and     $0F     ; get right shift count
    or      a
    jr      z, .doneRightShift
.rightShift
    srl     b
    rr      c
    dec     a
    jr      nz, .rightShift
.doneRightShift
    ld      a, d
    and     $F0     ; get left shift count
    swap    a
    or      a
    jr      z, .doneLeftShift
.leftShift
    sla     c
    rl      b
    dec     a
    jr      nz, .leftShift
.doneLeftShift
    ld      [hl], b
    inc     l
    ld      [hl], c
    ret

; INCr a -- b
instr_INCr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    jp      instr_INC.continue ; TODO: Move to use jr

; POPr a -- 
instr_POPr::
    ldh     a, [rst_ptr]
    dec     a
    ldh     [rst_ptr], a
    ret

; NIPr a b -- b
instr_NIPr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ldh     [rst_ptr], a
    jp      instr_NIP.continue ; TODO: Move to use jr

; SWPr a b -- b a
instr_SWPr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    jp      instr_SWP.continue ; TODO: Move to use jr

; ROTr a b c -- b c a
instr_ROTr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    jp      instr_ROT.continue ; TODO: Move to use jr

; DUPr a -- a a
instr_DUPr::
    RST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    RST_PTR_L
    ret

; OVRr a b -- a b a
instr_OVRr::
    RST_HL_dec
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; EQUr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to instr_EQU
instr_EQUr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nz, .notEqual
    inc     a
.notEqual
    ld      [hli], a
    RST_PTR_L
    ret

; NEQr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to instr_NEQ
instr_NEQr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      z, .equal
    inc     a
.equal
    ld      [hli], a
    RST_PTR_L
    ret

; GTHr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to instr_GTH
instr_GTHr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nc, .notGreater
    inc     a
.notGreater
    ld      [hli], a
    RST_PTR_L
    ret

; LTHr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to instr_LTH
instr_LTHr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      c, .notLesser
    jr      z, .notLesser
    inc     a
.notLesser
    ld      [hli], a
    RST_PTR_L
    ret

; JMPr addr --
instr_JMPr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ldh     [rst_ptr], a
    jp      instr_JMP.continue

; JCNr cond8 addr --
instr_JCNr::
    ld      h, HIGH(w_st)
    ldh     a, [wst_ptr]
    dec     a
    dec     a
    ldh     [wst_ptr], a
    jp      instr_JCN.continue

; JSRr addr --
instr_JSRr::
    ldh     a, [pc]
    ld      b, a
    ldh     a, [pc+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -uxn_memory
    add     hl, bc
    ld      b, h
    ld      c, l

    WST_HL
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ; UXN return address now on WST, continue with normal JMPr
    jr      instr_JMPr

; STHr a --
; TODO: Check if removing macros opens up optimizations
instr_STHr::
    RST_HL_dec
    ld      b, [hl]
    RST_PTR_L   ; destroys A
    WST_HL      ; destroys A
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; LDZr addr8 -- value
instr_LDZr::
    RST_HL_dec
    jp      instr_LDZ.continue

; STZr value addr8 --
instr_STZr::
    RST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    dec     l
    ld      a, [hl]
    ld      [bc], a
    RST_PTR_L
    ret

; LDRr addr8 -- value
instr_LDRr::
    RST_HL_dec
    jp      instr_LDR.continue

; STRr value addr8 --
instr_STRr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jp      instr_STR.continue

; LDAr addr16 -- value
instr_LDAr::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ldh     [rst_ptr], a
    jp      instr_LDA.continue

; STAr value addr16 --
instr_STAr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jp      instr_STA.continue

instr_DEIr::
    ret

; DEOr val device8 --
instr_DEOr::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    RST_PTR_L
    jp      instr_DEO.continue

; ADDr a b -- c
instr_ADDr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    ld      [hli], a
    RST_PTR_L
    ret

; SUBr a b -- c
instr_SUBr::
    RST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    dec     l
    ld      [hli], a
    RST_PTR_L
    ret

; MUrL a b -- c
instr_MULr::
    RST_HA_dec_ptr
    jp      instr_MUL.continue

; DIVr a b -- c
instr_DIVr::
    RST_HA_dec_ptr
    jp      instr_DIV.continue

; ANDr a b -- c
instr_ANDr::
    RST_HA_dec_ptr
    jp      instr_AND.continue

; ORAr a b -- c
instr_ORAr::
    RST_HA_dec_ptr
    jp      instr_ORA.continue

; EORr a b -- c
instr_EORr::
    RST_HA_dec_ptr
    jp      instr_EOR.continue

; SFTr a shift8 -- c
instr_SFTr::
    RST_HA_dec_ptr
    jp      instr_SFT.continue

; INC2r a -- b
instr_INC2r::
    RST_HL_dec
    jp      instr_INC2.continue

; POP2r a b -- 
instr_POP2r::
    ldh     a, [rst_ptr]
    dec     a
    dec     a
    ldh     [rst_ptr], a
    ret

; DUP2r a b -- a b a b
instr_DUP2r::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; NIP2r a b c d -- c d
instr_NIP2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L
    jp      instr_NIP2.continue

; SWP2r a b c d -- c d a b
instr_SWP2r::
    RST_HL_dec
    jp      instr_SWP2.continue

; OVR2r a b c d -- a b c d a b
instr_OVR2r::
    RST_HL_dec
    dec     l
    dec     l
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; ROT2r a b c d e f -- c d e f a b
instr_ROT2r::
    RST_HL_dec
    jp      instr_ROT2.continue

; EQU2r a b c d -- bool8
instr_EQU2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      nz, .notEqual
    ld      a, c
    cp      e
    jr      nz, .notEqual
    inc     h
.notEqual
    ld      a, h
    pop     hl
    ld      [hli], a
    RST_PTR_L
    ret

; NEQ2r a b c d -- bool8
instr_NEQ2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      z, .equal
    ld      a, c
    cp      e
    jr      z, .equal
    inc     h
.equal
    ld      a, h
    pop     hl
    ld      [hli], a
    RST_PTR_L
    ret

; GTH2r a b c d -- bool8
instr_GTH2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl
    ld      h, 0
    ld      a, b
    cp      d
    jr      c, .greaterThan     ; if d > b, de > bc
    jr      nz, .notGreaterThan      
    ld      a, c
    cp      e
    jr      nc, .notGreaterThan
.greaterThan
    inc     h
.notGreaterThan
    ld      a, h
    pop     hl
    ld      [hli], a
    RST_PTR_L
    ret

; LTH2r a b d c -- bool8
instr_LTH2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl
    ld      h, 0
    ld      a, d
    cp      b
    jr      c, .lessThan
    jr      nz, .notLessThan      
    ld      a, e
    cp      c
    jr      nc, .notLessThan
.lessThan
    inc     h
.notLessThan
    ld      a, h
    pop     hl
    ld      [hli], a
    RST_PTR_L
    ret

; JMP2r addr --
instr_JMP2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jp      instr_JMP2.continue ; TODO: Move calls to within jr range

; JCN2r cond addr --
instr_JCN2r::
    RST_HL_dec
    dec     l
    dec     l
    ld      c, [hl]
    RST_PTR_L
    ld      a, c
    or      a
    ret     z   ; condition not met
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    jp      instr_JMP2.jump

; JSR2r addr --
instr_JSR2r::
    WST_HL
    ldh     a, [pc]
    ld      b, a
    ldh     a, [pc+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -uxn_memory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    jr      instr_JMP2r

; STH2r a b --
instr_STH2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L   ; destroys A
    WST_HL      ; destroys A
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; LDZ2r addr8 -- value
; TODO: Consider all RST tweaking at the start so we can jump to instr_LDZ2 to save bytes
instr_LDZ2r::
    RST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    RST_PTR_L
    ret

; STZr value addr8 --
instr_STZ2r::
    RST_HL_dec
    ld      b, HIGH(zero_page)
    ld      c, [hl]
    inc     c
    dec     l
    ld      a, [hld]
    ld      [bc], a
    dec     c
    ld      a, [hl]
    ld      [bc], a
    RST_PTR_L
    ret

; LDR2r addr8 -- value
instr_LDR2r::
    RST_HL_dec
    ld      c, [hl]
    ; sign extension
    ld      a, c
    add     a       ; push sign into carry
    sbc     a       ; turn into 0 or -1
    ld      b, a    ; high byte
    push    hl
    PC_to_HL
    add     hl, bc
    ld      a, [hli]    ; TODO: Deal with SRAM banks
    ld      b, [hl]
    pop     hl
    ld      [hli], a
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; STR2r value addr8 --
instr_STR2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jp      instr_STR2.continue

; LDA2r addr16 -- value
instr_LDA2r::
    RST_HL_dec
    jp      instr_LDA2.continue

; STA2r value addr16 --
instr_STA2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jp      instr_STA2

instr_DEI2r::
    rst     Crash

; DEOr val device8 --
instr_DEO2r::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L
    jp      instr_DEO2.continue

; ADD2r a b -- c
instr_ADD2r::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [rst_ptr], a
    jp      instr_ADD2.continue

; SUB2r a b -- c
instr_SUB2r::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [rst_ptr], a
    jp      instr_SUB2.continue

; MUL2r a b c d -- e f
instr_MUL2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jp      instr_MUL2.continue

; DIV2r a b c d -- e f
instr_DIV2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jp      instr_DIV2.continue

; AND2r a b -- c
instr_AND2r::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [rst_ptr], a
    jp      instr_AND2.continue

; ORA2r a b -- c
instr_ORA2r::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [rst_ptr], a
    jp      instr_ORA2.continue

; EOR2r a b -- c
instr_EOR2r::
    ld      h, HIGH(r_st)
    ldh     a, [rst_ptr]
    dec     a
    ld      l, a
    dec     a
    ldh     [rst_ptr], a
    jp      instr_EOR2.continue

; SFT2r a shift8 -- c
instr_SFT2r::
    RST_HA_dec_ptr
    jp      instr_SFT2.continue

; LIT -- a
instr_LIT::
    PC_to_B
    inc     hl          ; increment PC, and store new value
    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a
    WST_HL
    ld      [hl], b     ; push onto wst
    inc     l           ; inc stack ptr
    WST_PTR_L
    ret

; INCk a -- a b
instr_INCk::
    WST_HL_dec
    ld      a, [hli]
    inc     a
    ld      [hli], a
    WST_PTR_L
    ret

; POPk a -- a
instr_POPk::
    ret

; DUPk a -- a a a
instr_DUPk::
    WST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    ld      [hli], a    ; TODO: Jump to instr_DUP's ld [hli], a to save bytes?
    WST_PTR_L
    ret

; NIPk a b -- a b
instr_NIPk::
    ret

instr_SWPk::
instr_OVRk::
instr_ROTk::
instr_EQUk::
instr_NEQk::
instr_GTHk::
instr_LTHk::
instr_JMPk::
instr_JCNk::
instr_JSRk::
instr_STHk::
instr_LDZk::
instr_STZk::
instr_LDRk::
instr_STRk::
    rst     Crash


; LDAk addr16 -- addr16 value
instr_LDAk::
    WST_HL_dec
    dec     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l

    BC_to_UXN_Banked

    ld      a, [bc]
    ld      [hli], a
    WST_PTR_L
    ret

instr_STAk::
instr_DEIk::
instr_DEOk::

; ADDk a b -- a b c
instr_ADDk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; SUBk a b -- a b c
instr_SUBk::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

instr_MULk::
instr_DIVk::

; ANDk a b -- a b c
instr_ANDk::
    WST_HL_dec
    ld      a, [hld]
    and     [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; ORAk a b -- a b c
instr_ORAk::
    WST_HL_dec
    ld      a, [hld]
    or      [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; EORk a b -- a b c
instr_EORk::
    WST_HL_dec
    ld      a, [hld]
    xor     [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; SFTk a shift8 -- a shift8 c
instr_SFTk::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    ld      a, b
    and     $0F     ; get right shift count
    or      a
    jr      z, .doneRightShift
.rightShift
    srl     c
    dec     a
    jr      nz, .rightShift
.doneRightShift
    ld      a, b
    and     $F0     ; get left shift count
    swap    a
    or      a
    jr      z, .doneLeftShift
.leftShift
    sla     c
    dec     a
    jr      nz, .leftShift
.doneLeftShift
    inc     l   ; TODO: Optimize hl traversal
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ret

; LIT2 -- a
instr_LIT2::
    PC_to_HL
    ld      b, [hl]
    inc     hl      ; must be a 16bit inc
    ld      c, [hl]
    inc     hl      ; must be a 16bit inc
    HL_to_PC
    WST_HL
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ret

instr_INC2k::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    inc     bc      ; inc value
    inc     l       ; seek to new wst position
    inc     l
    ld      [hl], b ; store new value
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ret

instr_POP2k::
instr_DUP2k::
instr_NIP2k::
instr_SWP2k::
instr_OVR2k::
instr_ROT2k::
instr_EQU2k::
instr_NEQ2k::
instr_GTH2k::
instr_LTH2k::
instr_JMP2k::
instr_JCN2k::
instr_JSR2k::
instr_STH2k::
instr_LDZ2k::
instr_STZ2k::
instr_LDR2k::
instr_STR2k::
instr_LDA2k::
instr_STA2k::
instr_DEI2k::
instr_DEO2k::
instr_ADD2k::
instr_SUB2k::
instr_MUL2k::
instr_DIV2k::
instr_AND2k::
instr_ORA2k::
instr_EOR2k::
instr_SFT2k::

instr_LITr::
instr_INCkr::
instr_POPkr::
instr_DUPkr::
instr_NIPkr::
instr_SWPkr::
instr_OVRkr::
instr_ROTkr::
instr_EQUkr::
instr_NEQkr::
instr_GTHkr::
instr_LTHkr::
instr_JMPkr::
instr_JCNkr::
instr_JSRkr::
instr_STHkr::
instr_LDZkr::
instr_STZkr::
instr_LDRkr::
instr_STRkr::
instr_LDAkr::
instr_STAkr::
instr_DEIkr::
instr_DEOkr::
instr_ADDkr::
instr_SUBkr::
instr_MULkr::
instr_DIVkr::
instr_ANDkr::
instr_ORAkr::
instr_EORkr::
instr_SFTkr::

instr_LIT2r::
instr_INC2kr::
instr_POP2kr::
instr_DUP2kr::
instr_NIP2kr::
instr_SWP2kr::
instr_OVR2kr::
instr_ROT2kr::
instr_EQU2kr::
instr_NEQ2kr::
instr_GTH2kr::
instr_LTH2kr::
instr_JMP2kr::
instr_JCN2kr::
instr_JSR2kr::
instr_STH2kr::
instr_LDZ2kr::
instr_STZ2kr::
instr_LDR2kr::
instr_STR2kr::
instr_LDA2kr::
instr_STA2kr::
instr_DEI2kr::
instr_DEO2kr::
instr_ADD2kr::
instr_SUB2kr::
instr_MUL2kr::
instr_DIV2kr::
instr_AND2kr::
instr_ORA2kr::
instr_EOR2kr::
instr_SFT2kr::
    rst     Crash
