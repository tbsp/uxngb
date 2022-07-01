;
; Core CPU Instructions for uxnemu
;
; Copyright 2022 Dave VanEe
;
; This software is provided 'as-is', without any express or implied
; warranty.  In no event will the authors be held liable for any damages
; arising from the use of this software.
; 
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
; 
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.
;

SECTION "DIV2 WRAM", WRAM0, ALIGN[3]
_MD16temp:  ds 2
_MD16count: db

SECTION "UXN Instructions", ROM0

; We take advantage of the fact these are next to eachother for some optimizations
ASSERT(HIGH(wRST) == HIGH(wWST) + 1)

; UXN Instruction Implementations

; Support macros
WST_HL: MACRO ; instructions which add to the stack start at ptr
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ENDM

WST_HL_dec: MACRO ; instructions which consume the stack start at ptr-1
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    dec     l
    ENDM

WST_PTR_L: MACRO
    ld      a, l
    ldh     [hWSTPtr], a
    ENDM

WST_HA_dec_ptr: MACRO
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ldh     [hWSTPtr], a
    ENDM

RST_HL: MACRO
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ENDM

RST_HL_dec: MACRO ; instructions which consume the stack start at ptr-1
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    dec     l
    ENDM

RST_PTR_L: MACRO
    ld      a, l
    ldh     [hRSTPtr], a
    ENDM

RST_HA_dec_ptr: MACRO
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    ENDM

WBIT_2K_SETUP: MACRO
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     6
    ld      l, a
    ENDM

WMATH_2K_SETUP: MACRO
    WBIT_2K_SETUP
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    ENDM

RBIT_2K_SETUP: MACRO
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    inc     a
    ldh     [hRSTPtr], a
.continue
    sub     6
    ld      l, a
    ENDM

RMATH_2K_SETUP: MACRO
    RBIT_2K_SETUP
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    ENDM

PC_to_HL: MACRO
    ldh     a, [hPC]
    ld      h, a
    ldh     a, [hPC+1]
    ld      l, a
    ENDM

HL_to_PC: MACRO
    ld      a, h
    ldh     [hPC], a
    ld      a, l
    ldh     [hPC+1], a
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


_BRK::
    pop     af          ; pop off return address so we break out of uxn_eval on ret
_NIL::
    ret

; LIT -- a
_LIT::
    ; Thanks jvsTSX for help optimizing the PC increment code!

    ld      hl, hPC     ; get current PC
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a

    ld      d, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      e, a
    
    ld      a, [bc]     ; literal value

    inc     bc          ; increment PC
    ld      [hl], c     ; save updated PC
    dec     l
    ld      [hl], b

    ld      [de], a     ; push onto wst
    inc     e           ; inc stack ptr

    ld      a, e
    ldh     [hWSTPtr], a

    ret

; LITr -- a
_LITr::
    ld      hl, hPC     ; get current PC
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a

    ld      d, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      e, a
    
    ld      a, [bc]     ; literal value

    inc     bc          ; increment PC
    ld      [hl], c     ; save updated PC
    dec     l
    ld      [hl], b

    ld      [de], a     ; push onto wst
    inc     e           ; inc stack ptr

    ld      a, e
    ldh     [hRSTPtr], a

    ret

; LIT2 -- a
_LIT2::
    ld      hl, hPC     ; get current PC
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a

    ld      a, [bc]     ; read literal short into DE
    ld      d, a
    inc     bc
    ld      a, [bc]
    ld      e, a
    inc     bc

    ld      [hl], c     ; save updated PC
    dec     l
    ld      [hl], b

    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a

    ld      [hl], d
    inc     l
    ld      [hl], e
    inc     l

    ld      a, l
    ldh     [hWSTPtr], a

    ret

; LIT2r -- a
_LIT2r::
    ld      hl, hPC     ; get current PC
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a

    ld      a, [bc]     ; read literal short into DE
    ld      d, a
    inc     bc
    ld      a, [bc]
    ld      e, a
    inc     bc

    ld      [hl], c     ; save updated PC
    dec     l
    ld      [hl], b

    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a

    ld      [hl], d
    inc     l
    ld      [hl], e
    inc     l

    ld      a, l
    ldh     [hRSTPtr], a
    ret

; INC a -- b
_INC::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
.continue
    ld      l, a
    dec     l
    inc     [hl]
    ret

; INCr a -- b
_INCr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    jr      _INC.continue

; INCk a -- a b
_INCk::
    WST_HL_dec
    ld      a, [hli]
    inc     a
    ld      [hli], a
    WST_PTR_L
    ret

; INCkr a -- a b
_INCkr::
    RST_HL_dec
    ld      a, [hli]
    inc     a
    ld      [hli], a
    RST_PTR_L
    ret

; INC2 a -- b
_INC2::
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
    
; INC2r a -- b
_INC2r::
    RST_HL_dec
    jr      _INC2.continue

; INC2k a -- a b
_INC2k::
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

; INC2kr a -- a b
_INC2kr::
    RST_HL_dec
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
    RST_PTR_L
    ret

; POP a -- 
_POP::
    ldh     a, [hWSTPtr]
    dec     a
    ldh     [hWSTPtr], a
    ret

; POPr a -- 
_POPr::
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    ret

; POPk a -- a
_POPk::
    ; TODO: Underflow error if stack if empty
    ret

; POPkr a -- a
_POPkr::
    ; TODO: Underflow error if stack if empty
    ret

; POP2 a b -- 
_POP2::
    ldh     a, [hWSTPtr]
    dec     a
    dec     a
    ldh     [hWSTPtr], a
    ret

; POP2r a b -- 
_POP2r::
    ldh     a, [hRSTPtr]
    dec     a
    dec     a
    ldh     [hRSTPtr], a
    ret

; POP2k a b -- a b
_POP2k::
    ; TODO: Underflow error if stack if empty
    ret

; POP2kr a b -- a b
_POP2kr::
    ; TODO: Underflow error if rst if empty
    ret

; NIP a b -- b
_NIP::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ldh     [hWSTPtr], a
.continue
    ld      l, a
    ld      a, [hld]
    ld      [hli], a
    ret

; NIPr a b -- b
_NIPr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    jr      _NIP.continue

; NIPk a b -- a b b
_NIPk::
    jp      _DUP

; NIPkr a b -- a b b
_NIPkr::
    jp      _DUPr

; NIP2 a b c d -- c d
_NIP2::
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

; NIP2r a b c d -- c d
_NIP2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L
    jr      _NIP2.continue

; NIP2k a b -- a b b
_NIP2k::
    jp      _DUP2

; NIP2kr a b -- a b b
_NIP2kr::
    jp      _DUP2r

; SWP a b -- b a
_SWP::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
.continue
    ld      l, a
    dec     l
    ld      a, [hld]
    ld      b, [hl]
    ld      [hli], a
    ld      [hl], b
    ret
    
; SWPr a b -- b a
_SWPr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    jr      _SWP.continue

; SWPk a b -- a b b a
_SWPk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; SWPkr a b -- a b b a
_SWPkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; SWP2 a b c d -- c d a b
_SWP2::
    WST_HL_dec
.continue
    ld      a, [hld]
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]
    ld      [hl], c
    inc     l
    ld      [hli], a
    ld      [hl], e
    inc     l
    ld      [hl], d
    ret

; SWP2r a b c d -- c d a b
_SWP2r::
    RST_HL_dec
    jr      _SWP2.continue

; SWP2k a b c d -- a b c d c d a b
_SWP2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    add     4
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      d, [hl]
    dec     l
    ld      e, [hl]

    ld      l, a
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    ld      [hl], e
    inc     l
    ld      [hl], d
    ret

; SWP2kr a b c d -- a b c d c d a b
_SWP2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    add     4
    ldh     [hRSTPtr], a
    jr      _SWP2k.continue

; ROT a b c -- b c a
_ROT::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
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

; ROTr a b c -- b c a
_ROTr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    jr      _ROT.continue

; ROTk a b c -- a b c b c a
_ROTk::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      a, [hld]
    ld      c, [hl]
    inc     l
    inc     l
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ret

; ROTkr a b c -- a b c b c a
_ROTkr::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      a, [hld]
    ld      c, [hl]
    inc     l
    inc     l
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    ret

; ROT2 a b c d e f -- c d e f a b
_ROT2::
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

; ROT2r a b c d e f -- c d e f a b
_ROT2r::
    RST_HL_dec
    jr      _ROT2.continue

; ROT2k a b c d e f -- a b c d e f c d e f a b
; TODO: Try to speed up this register-pressure traversal
_ROT2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    add     6
    ldh     [hWSTPtr], a
.continue
    sub     12          ; jump to the start of a, b
    ld      l, a

    ld      b, [hl]     ; get a, b
    inc     l
    ld      c, [hl]
    ld      a, l        ; jump to the end
    add     9
    ld      l, a
    ld      [hl], b     ; store a, b
    inc     l
    ld      [hl], c

    ld      a, l        ; jump to the start of c, d, e, f
    sub     9
    ld      l, a
    ld      b, [hl]     ; get c, d, e, f
    inc     l
    ld      c, [hl]
    inc     l
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l

    ld      [hl], b     ; store c, d, e, f
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], d
    inc     l
    ld      [hl], e
    inc     l

    ret

; ROT2kr a b c d e f -- a b c d e f c d e f a b
; TODO: Try to speed up this register-pressure traversal
_ROT2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    add     6
    ldh     [hRSTPtr], a
    jr      _ROT2k.continue

; DUP a -- a a
_DUP::
    WST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    WST_PTR_L
    ret

; DUPr a -- a a
_DUPr::
    RST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    RST_PTR_L
    ret

; DUPk a -- a a a
_DUPk::
    WST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    ld      [hli], a    ; TODO: Jump to _DUP's ld [hli], a to save bytes?
    WST_PTR_L
    ret

; DUPkr a -- a a a
_DUPkr::
    RST_HL_dec
    ld      a, [hli]
    ld      [hli], a
    ld      [hli], a    ; TODO: Jump to _DUP's ld [hli], a to save bytes?
    RST_PTR_L
    ret

; DUP2 a b -- a b a b
_DUP2::
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

; DUP2r a b -- a b a b
_DUP2r::
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

; DUP2k a b -- a b a b a b
_DUP2k::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; DUP2kr a b -- a b a b a b
_DUP2kr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    inc     l
    inc     l
    ld      [hl], b
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; OVR a b -- a b a
_OVR::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; OVRr a b -- a b a
_OVRr::
    RST_HL_dec
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; OVRk a b -- a b a b a
_OVRk::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; OVRkr a b -- a b a b a
_OVRkr::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      a, [hli]
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; OVR2 a b c d -- a b c d a b
_OVR2::
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

; OVR2r a b c d -- a b c d a b
_OVR2r::
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

; OVR2k a b c d -- a b c d a b c d a b
_OVR2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    add     6
    ldh     [hWSTPtr], a
.continue
    sub     10
    ld      l, a

    ld      a, [hli]    ; read starting values
    ld      c, [hl]
    inc     l
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l

    ld      [hli], a    ; store keep copy
    ld      [hl], c
    inc     l
    ld      [hl], d
    inc     l
    ld      [hl], e
    inc     l

    ld      [hli], a    ; store OVR short
    ld      [hl], c

    ret

; OVR2kr a b c d -- a b c d a b c d a b
_OVR2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    add     6
    ldh     [hRSTPtr], a
    jr      _OVR2k.continue

; EQU a b -- bool8
_EQU::
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

; EQUr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to _EQU
_EQUr::
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

; EQUk a b -- a b bool8
_EQUk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nz, .notEqual
    inc     a
.notEqual
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; EQUkr a b -- a b bool8
_EQUkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nz, .notEqual
    inc     a
.notEqual
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; EQU2 a b c d -- bool8
_EQU2::
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

; EQU2r a b c d -- bool8
_EQU2r::
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

; EQU2k a b c d -- a b c d bool8
_EQU2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a

    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
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
    ld      [hl], a
    ret

; EQU2kr a b c d -- a b c d bool8
_EQU2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    jr      _EQU2k.continue

; NEQ a b -- bool8
_NEQ::
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

; NEQr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to _NEQ
_NEQr::
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

; NEQk a b -- a b bool8
_NEQk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      z, .equal
    inc     a
.equal
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; NEQkr a b -- a b bool8
_NEQkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      z, .equal
    inc     a
.equal
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; NEQ2 a b c d -- bool8
_NEQ2::
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

; NEQ2r a b c d -- bool8
_NEQ2r::
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

; NEQ2k a b c d -- a b c d bool8
_NEQ2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a

    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
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
    ld      [hl], a
    ret

; NEQ2kr a b c d -- a b c d bool8
_NEQ2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    jr      _NEQ2k.continue

; GTH a b -- bool8
_GTH::
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

; GTHr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to _GTH
_GTHr::
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

; GTHk a b -- a b bool8
_GTHk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nc, .notGreater
    inc     a
.notGreater
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; GTHkr a b -- a b bool8
_GTHkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      nc, .notGreater
    inc     a
.notGreater
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; GTH2 a b c d -- bool8
_GTH2::
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

; GTH2r a b c d -- bool8
_GTH2r::
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

; GTH2k a b c d -- a b c d bool8
_GTH2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a

    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
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
    ld      [hl], a
    ret

; GTH2kr a b c d -- a b c d bool8
_GTH2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    jr      _GTH2k.continue

; LTH a b -- bool8
_LTH::
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
; LTHr a b -- bool8
; TODO: Optimize to RSL_PTR_L earlier and jump to _LTH
_LTHr::
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

; LTHk a b -- a b bool8
_LTHk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      c, .notLesser
    jr      z, .notLesser
    inc     a
.notLesser
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; LTHkr a b -- a b bool8
_LTHkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    cp      b
    ld      a, 0
    jr      c, .notLesser
    jr      z, .notLesser
    inc     a
.notLesser
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; LTH2 a b d c -- bool8
_LTH2::
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

; LTH2r a b d c -- bool8
_LTH2r::
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

; LTH2k a b d c -- a b d c bool8
_LTH2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a

    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
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
    ld      [hl], a
    ret

; LTH2kr a b d c -- a b d c bool8
_LTH2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    jr      _LTH2k.continue

; The JCN set of instructions is somewhat oddly positioned since it's expected
;  to be a high-use instruction and being within JR range of _JMP is nice

; JCN cond8 addr --
_JCN::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    dec     a
    ldh     [hWSTPtr], a
.continue
    ld      l, a
    ld      c, [hl]
    ld      a, c
    or      a
    ret     z   ; condition not met
    inc     l
    ld      c, [hl]
    jr      _JMP.jump

; JCNr cond8 addr --
_JCNr::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    dec     a
    ldh     [hWSTPtr], a
    jr      _JCN.continue

; JCNk cond8 addr -- cond8 addr
_JCNk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    dec     a
    jr      _JCN.continue

; JCNkr cond8 addr -- cond8 addr
_JCNkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    dec     a
    jr      _JCN.continue

; JSR addr --
_JSR::
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
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
    ; fall through to _JMP
    ;jr      _JMP

; JMP addr --
_JMP::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ldh     [hWSTPtr], a
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
    ldh     [hPC], a
    ld      a, l
    ldh     [hPC+1], a
    ret

; JSRr addr --
_JSRr::
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
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
    ; Fall through to _JMPr
    ;jr      _JMPr

; JMPr addr --
_JMPr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    jr      _JMP.continue

; JSRk addr -- addr
_JSRk::
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l

    RST_HL
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    ; UXN return address now on RST, continue with normal JMPk
    ; Fall through to _JMPk
    ;jr      _JMPk

; JMPk addr -- addr
_JMPk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    jr      _JMP.continue

; JSRkr addr -- addr
_JSRkr::
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l

    WST_HL
    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ; UXN return address now on RST, continue with normal JMPkr
    ; Fall through to _JMPkr
    ;jr      _JMPkr

; JMPkr addr -- addr
_JMPkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    jr      _JMP.continue

; JCN2 cond addr --
_JCN2::
    WST_HL_dec
    dec     l
    dec     l
    ld      c, [hl]
    WST_PTR_L
.continue
    ld      a, c
    or      a
    ret     z   ; condition not met
    inc     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    jr      _JMP2.jump

; JCN2r cond addr --
_JCN2r::
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
    jr      _JMP2.jump

; JCN2k cond addr -- cond addr
_JCN2k::
    WST_HL_dec
    dec     l
    dec     l
    ld      c, [hl]
    jr      _JCN2.continue

; JCN2kr cond addr -- cond addr
_JCN2kr::
    RST_HL_dec
    dec     l
    dec     l
    ld      c, [hl]
    jr      _JCN2.continue

; JSR2 addr --
_JSR2::
    RST_HL
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    ; Fall through to _JMP2
    ;jr      _JMP2

; JMP2 addr --
_JMP2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]
.jump
    ld      hl, eUXNMemory
    add     hl, bc
    ld      a, h
    ldh     [hPC], a
    ld      a, l
    ldh     [hPC+1], a
    ret

; JSR2r addr --
_JSR2r::
    WST_HL
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ; Fall through to _JMP2r
    ;jr      _JMP2r

; JMP2r addr --
_JMP2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jr      _JMP2.continue

; JSR2k addr -- addr
_JSR2k::
    ; TODO: Find a way to reuse _JSR2, which is identical except for where it jumps at the end
    RST_HL
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    RST_PTR_L
    ; Fall through to JMP2k
    ;jr      _JMP2k

; JMP2k addr -- addr
_JMP2k::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    jr      _JMP2.continue

; JSR2kr addr -- addr
_JSR2kr::
    ; TODO: Find a way to reuse _JSR2?, which is identical except for where it jumps at the end
    WST_HL
    ldh     a, [hPC]
    ld      b, a
    ldh     a, [hPC+1]
    ld      c, a

    push    hl
    ; Offset back to UXN address, in case someone does some direct manipulation
    ; TODO: Account for banking!
    ld      hl, $ffff & -eUXNMemory
    add     hl, bc
    ld      b, h
    ld      c, l
    pop     hl

    ld      [hl], b
    inc     l
    ld      [hl], c
    inc     l
    WST_PTR_L
    ; Fall through to _JMP2kr
    ;jr      _JMP2kr

; JMP2kr addr -- addr
_JMP2kr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    jr      _JMP2.continue
    
; STH a --
_STH::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ldh     [hWSTPtr], a
    ld      l, a
    ld      b, [hl]
    inc     h   ; point to HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; STHr a --
_STHr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    ld      l, a
    ld      b, [hl]
    dec     h   ; point to HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; STHk a -- a
_STHk::
    WST_HL_dec
    ld      b, [hl]
    inc     h   ; point to HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; STHkr a -- a
_STHkr::
    RST_HL_dec
    ld      b, [hl]
    dec     h   ; point to HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; STH2 a b --
_STH2::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    WST_PTR_L   ; destroys A
.continue
    inc     h   ; point to HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; STH2r a b --
_STH2r::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L   ; destroys A
    dec     h   ; point to HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; STH2k a b -- a b
_STH2k::
    WST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    jr      _STH2.continue

; STH2kr a b -- a b
_STH2kr::
    RST_HL_dec
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    dec     h   ; point to HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      [hl], c
    inc     l
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; LDZ addr8 -- value
_LDZ::
    WST_HL_dec
.continue
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hl], a
    ret

; LDZr addr8 -- value
_LDZr::
    RST_HL_dec
    jr      _LDZ.continue

; LDZk addr8 -- addr8 value
_LDZk::
    WST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; LDZkr addr8 -- addr8 value
_LDZkr::
    RST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; LDZ addr8 -- value
_LDZ2::
    WST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    WST_PTR_L
    ret

; LDZ2r addr8 -- value
; TODO: Consider all RST tweaking at the start so we can jump to _LDZ2 to save bytes
_LDZ2r::
    RST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    RST_PTR_L
    ret

; LDZk addr8 -- addr8 value
_LDZ2k::
    WST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    inc     l
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    WST_PTR_L
    ret

; LDZkr addr8 -- addr8 value
_LDZ2kr::
    RST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    ld      a, [bc]
    inc     l
    ld      [hli], a
    inc     c
    ld      a, [bc]
    ld      [hli], a
    RST_PTR_L
    ret

; STZ value addr8 --
_STZ::
    WST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    dec     l
    ld      a, [hl]
    ld      [bc], a
    WST_PTR_L
    ret

; STZr value addr8 --
_STZr::
    RST_HL_dec
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    dec     l
    ld      a, [hl]
    ld      [bc], a
    RST_PTR_L
    ret

; STZk value addr8 -- value addr8
_STZk::
    WST_HL_dec
.continue
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    dec     l
    ld      a, [hl]
    ld      [bc], a
    ret

; STZkr value addr8 -- value addr8
_STZkr::
    RST_HL_dec
    jr      _STZk

; STZ value addr8 --
_STZ2::
    WST_HL_dec
    ld      b, HIGH(eZeroPage)
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

; STZr value addr8 --
_STZ2r::
    RST_HL_dec
    ld      b, HIGH(eZeroPage)
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

; STZk value addr8 -- value addr8
_STZ2k::
    ; TODO: Same as _STZ2 without the final WST_PTR_L, any way to reuse?
    WST_HL_dec
.continue
    ld      b, HIGH(eZeroPage)
    ld      c, [hl]
    inc     c
    dec     l
    ld      a, [hld]
    ld      [bc], a
    dec     c
    ld      a, [hl]
    ld      [bc], a
    ret

; STZkr value addr8 -- value addr8
_STZ2kr::
    RST_HL_dec
    jr      _STZ2k.continue

; LDR addr8 -- value
_LDR::
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

; LDRr addr8 -- value
_LDRr::
    RST_HL_dec
    jr      _LDR.continue

; LDRk addr8 -- addr8 value
_LDRk::
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
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; LDRkr addr8 -- addr8 value
_LDRkr::
    RST_HL_dec ; TODO: Use pre-ptr-inc to allow jump to _LDRk.continue
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
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; LDR2 addr8 -- value
_LDR2::
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

; LDR2r addr8 -- value
_LDR2r::
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

; LDR2k addr8 -- addr8 value
_LDR2k::
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
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    WST_PTR_L
    ret

; LDR2kr addr8 -- addr8 value
_LDR2kr::
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
    inc     l
    ld      [hli], a
    ld      [hl], b
    inc     l
    RST_PTR_L
    ret

; STR value addr8 --
_STR::
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

; STRr value addr8 --
_STRr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jr      _STR.continue

; STRk value addr8 -- value addr8
_STRk::
    WST_HL_dec
.continue
    ld      c, [hl]
    dec     l
    ld      d, [hl]
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

; STRkr value addr8 -- value addr8
_STRkr::
    RST_HL_dec
    jr      _STRk.continue

; STR2 value addr8 --
_STR2::
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

; STR2r value addr8 --
_STR2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jr      _STR2.continue

; STR2k value addr8 -- value addr8
_STR2k::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    jr      _STR2.continue

; STR2kr value addr8 -- value addr8
_STR2kr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    jr      _STR2.continue

; LDA addr16 -- value
_LDA::
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

; LDAr addr16 -- value
_LDAr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ldh     [hRSTPtr], a
    jr      _LDA.continue

; LDAk addr16 -- addr16 value
_LDAk::
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

; LDAkr addr16 -- addr16 value
; TODO: Use pre-ptr-inc to allow jump to _LDAk.continue
_LDAkr::
    RST_HL_dec
    dec     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l

    BC_to_UXN_Banked

    ld      a, [bc]
    ld      [hli], a
    RST_PTR_L
    ret

; LDA2 addr16 -- value
_LDA2::
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

; LDA2r addr16 -- value
_LDA2r::
    RST_HL_dec
    jr      _LDA2.continue

; LDA2k addr16 -- addr16 value
_LDA2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    inc     a
    inc     a
    ldh     [hWSTPtr], a
.continue
    dec     l
    dec     l
    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    BC_to_UXN_Banked
    ld      a, [bc]
    ld      [hli], a
    inc     bc      ; TODO: Handle bank wrapping
    ld      a, [bc]
    ld      [hl], a
    ret

; LDA2kr addr16 -- addr16 value
_LDA2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    inc     a
    inc     a
    ldh     [hRSTPtr], a
    jr      _LDA2k.continue

; STA value addr16 --
_STA::
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

; STAr value addr16 --
_STAr::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jr      _STA.continue

; STAk value addr16 -- value addr16
_STAk::
    WST_HL_dec
.continue
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      d, [hl]
    BC_to_UXN_Banked
    ld      a, d
    ld      [bc], a
    ret

; STAkr value addr16 -- value addr16
_STAkr::
    RST_HL_dec
    jr      _STAk.continue

; STA2 value addr16 --
_STA2::
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

; STA2r value addr16 --
_STA2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    RST_PTR_L
    jr      _STA2.continue

; STA2k value addr16 -- value addr16
_STA2k::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
.continue
    BC_to_UXN_Banked
    ld      a, d
    ld      [bc], a
    inc     bc      ; TODO: Handle bank wrapping
    ld      a, e
    ld      [bc], a
    ret

; STA2kr value addr16 -- value addr16
_STA2kr::
    RST_HL_dec
    ; TODO: Reduce duplication before jump?
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    jr      _STA2k.continue

; DEI device8 -- value
_DEI::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
.early_continue
    ld      e, a
    dec     e
.continue
    ld      l, a
    dec     l
    ld      d, [hl]

    ; DEVPEEK8
    push    hl
    ld      hl, wDevices
    ld      a, d
    add     l
    ld      l, a

    ld      a, [hl]
    pop     hl
    ld      l, e    ; stack store location
    ld      [hl], a

    ; get handler address
    ld      a, d
    and     $F0
    rrca
    ;add     0       ; DEI handler offset
    ld      hl, DeviceHandlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; DEIr device8 -- value
_DEIr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    jr      _DEI.early_continue

; DEIk device8 -- device8 value
_DEIk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      e, a
    inc     a
    ldh     [hWSTPtr], a
    sub     2
    jr      _DEI.continue

; DEIkr device8 -- device8 value
_DEIkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      e, a
    inc     a
    ldh     [hRSTPtr], a
    sub     2
    jr      _DEI.continue

; DEI2 device8 -- value
_DEI2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      e, a
    dec     e
    inc     a
    ldh     [hWSTPtr], a
.continue
    dec     l
    ld      d, [hl]

    ; DEVPEEK16
    push    hl
    ld      hl, wDevices
    ld      a, d
    add     l
    ld      l, a

    ld      a, [hli]
    ld      b, [hl]
    pop     hl
    ld      l, e    ; stack store location
    ld      [hli], a
    ld      [hl], b

    ; get handler address
    ld      a, d
    and     $F0
    rrca
    add     2       ; DEI2 handler offset
    ld      hl, DeviceHandlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; DEI2r device8 -- value
_DEI2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ld      e, a
    dec     e
    inc     a
    ldh     [hRSTPtr], a
    jr      _DEI2.continue

; DEI2k device8 -- device8 value
_DEI2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    ld      l, a
    ld      e, a
    inc     a
    inc     a
    ldh     [hWSTPtr], a
    jr      _DEI2.continue

; DEI2kr device8 -- device8 value
_DEI2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    ld      l, a
    ld      e, a
    inc     a
    inc     a
    ldh     [hRSTPtr], a
    jr      _DEI2.continue

; DEO value device8 --
_DEO::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      b, [hl]

    ; DEVPOKE8
    ld      hl, wDevices
    ld      a, d
    add     l
    ld      l, a

    ld      [hl], b

    ; get handler address
    ld      a, d
    and     $F0
    rrca
    add     4       ; DEO handler offset
    ld      hl, DeviceHandlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; DEOr val device8 --
_DEOr::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    RST_PTR_L
    jr      _DEO.continue

; DEOk value device8 -- value device8
_DEOk::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    jr      _DEO.continue

; DEOkr value device8 -- value device8
_DEOkr::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    jr      _DEO.continue

; DEO value device8 --
_DEO2::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    WST_PTR_L
.continue
    ; DEVPOKE16
    ld      hl, wDevices
    ld      a, d
    add     l
    ld      l, a

    ld      [hl], b
    inc     l
    ld      [hl], c

    ; get handler address
    ld      a, d
    and     $F0
    rrca
    add     6       ; DEO2 handler offset
    ld      hl, DeviceHandlers
    add     l
    ld      l, a    ; LUT uses ALIGN[7], so no need to worry about carry
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL

    ret

; DEOr val device8 --
_DEO2r::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    RST_PTR_L
    jr      _DEO2.continue

; DEO2k value device8 -- value device8
_DEO2k::
    WST_HL_dec
    ld      d, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    jr      _DEO2.continue

; DEO2kr value device8 -- value device8
_DEO2kr::
    RST_HL_dec
    ld      d, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      c, [hl]
    jr      _DEO2.continue

; ADD a b -- c
_ADD::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    ld      [hli], a
    WST_PTR_L
    ret

; ADDr a b -- c
_ADDr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    ld      [hli], a
    RST_PTR_L
    ret

; ADDk a b -- a b c
_ADDk::
    WST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; ADDkr a b -- a b c
_ADDkr::
    RST_HL_dec
    ld      a, [hld]
    ld      b, [hl]
    add     b
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; ADD2 a b -- c
_ADD2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hWSTPtr], a
.continue
    ld      c, [hl]
    dec     l
    ld      b, [hl]
    dec     l
    ld      a, [hld]
    ld      d, [hl]
    push    hl
    ld      h, d
    ld      l, a
    add     hl, bc
    ld      a, h
    ld      e, l
    pop     hl
    ld      [hli], a
    ld      [hl], e
    ret

; ADD2r a b -- c
_ADD2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hRSTPtr], a
    jr      _ADD2.continue

; ADD2k a b -- a b c
_ADD2k::
    WMATH_2K_SETUP
.finally
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

; ADD2kr a b -- a b c
_ADD2kr::
    RMATH_2K_SETUP
    jr      _ADD2k.finally

; SUB a b -- c
_SUB::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    dec     l
    ld      [hli], a
    WST_PTR_L
    ret

; SUBr a b -- c
_SUBr::
    RST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    dec     l
    ld      [hli], a
    RST_PTR_L
    ret

; SUBk a b -- a b c
_SUBk::
    WST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; SUBkr a b -- a b c
_SUBkr::
    RST_HL_dec
    dec     l
    ld      a, [hli]
    sub     [hl]
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; SUB2 a b -- c
_SUB2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hWSTPtr], a
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

; SUB2r a b -- c
_SUB2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hRSTPtr], a
    jr      _SUB2.continue

; SUB2k a b -- a b c
_SUB2k::
    WMATH_2K_SETUP
.finally
    ld      a, e
    sub     c
    ld      e, a
    ld      a, d
    sbc     b
    ld      [hli], a
    ld      [hl], e
    ret

; SUB2kr a b -- a b c
_SUB2kr::
    RMATH_2K_SETUP
    jr      _SUB2k.finally

; MUL a b -- c
_MUL::
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

; MUrL a b -- c
_MULr::
    RST_HA_dec_ptr
    jr      _MUL.continue

; MULk a b -- a b c
_MULk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
    dec     a
    dec     a
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
    inc     l
    inc     l
    ld      [hl], a
    ret

; MULkr a b -- a b c
_MULkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    dec     a
    dec     a
    jr      _MULk.continue

; MUL2r a b c d -- e f
_MUL2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jr      _MUL2.continue

; MUL2 a b c d -- e f
_MUL2::
    WST_HL_dec
    ld      c, [hl]
    dec     l
    WST_PTR_L
.continue
    ld      a, [hld]
    ld      e, [hl]
    dec     l
    ld      d, [hl]
    push    hl

    ; http://map.grauw.nl/articles/mult_div_shifts.php

    REPT 16
    add     hl, hl
    sla     c
    rla
    jr      nc, :+
    add     hl,de
:
    ENDR

    ld      a, h
    ld      b, l

    pop     hl
    ld      [hli], a
    ld      [hl], b
    ret

; MUL2kr a b c d -- a b c d e f
_MUL2kr::
    RBIT_2K_SETUP
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      a, [hli]
    ld      c, [hl]
    inc     l
    jr      _MUL2k.finally

; MUL2k a b c d -- a b c d e f
_MUL2k::
    ; Macro unrolled to optimize
    WBIT_2K_SETUP
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    inc     l
    ld      a, [hli]
    ld      c, [hl]
    inc     l
.finally
    push    hl

    ; http://map.grauw.nl/articles/mult_div_shifts.php

    REPT 16
    add     hl, hl
    sla     c
    rla
    jr      nc, :+
    add     hl,de
:
    ENDR

    ld      a, h
    ld      b, l

    pop     hl
    ld      [hli], a
    ld      [hl], b
    ret

; DIV a b -- c
_DIV::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      c, [hl]
    dec     l
    ld      e, [hl]

    ; Source: http://map.grauw.nl/articles/mult_div_shifts.php
    xor     a

    REPT 8
    rl      e
    rla
    sub     c
    jr      nc, :+
    add     a, c
:
    ENDR

    ld      a, e
    rla
    cpl

    ld      [hl], a
    ret

; DIVr a b -- c
_DIVr::
    RST_HA_dec_ptr
    jr      _DIV.continue

; DIVk a b -- a b c
_DIVk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
    dec     a
    dec     a
.continue
    ld      l, a
    ld      c, [hl]
    dec     l
    ld      e, [hl]

    ; Source: http://map.grauw.nl/articles/mult_div_shifts.php
    xor     a

    REPT 8
    rl      e
    rla
    sub     c
    jr      nc, :+
    add     a, c
:
    ENDR

    ld      a, e
    rla
    cpl

    inc     l
    inc     l
    ld      [hl], a
    ret

; DIVkr a b -- a b c
_DIVkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    dec     a
    dec     a
    jr      _DIVk.continue

; DIV2 a b c d -- e f
_DIV2::
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

; DIV2r a b c d -- e f
_DIV2r::
    RST_HL_dec
    ld      c, [hl]
    dec     l
    RST_PTR_L
    jr      _DIV2.continue

; DIV2k a b c d -- a b c d e f
_DIV2k::
    WMATH_2K_SETUP
.finally
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

; DIV2kr a b c d -- a b c d e f
_DIV2kr::
    RMATH_2K_SETUP
    jr      _DIV2k.finally

; AND a b -- c
_AND::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    and     [hl]
    ld      [hl], a
    ret

; ANDr a b -- c
_ANDr::
    RST_HA_dec_ptr
    jr      _AND.continue

; ANDk a b -- a b c
_ANDk::
    WST_HL_dec
    ld      a, [hld]
    and     [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; ANDkr a b -- a b c
_ANDkr::
    RST_HL_dec
    ld      a, [hld]
    and     [hl]
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; AND2 a b -- c
_AND2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hWSTPtr], a
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

; AND2r a b -- c
_AND2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hRSTPtr], a
    jr      _AND2.continue

; AND2k a b -- a b c
_AND2k::
    WBIT_2K_SETUP
    ld      a, [hli]
    ld      b, [hl]
    inc     l
    and     [hl]
    ld      c, a    ; result of high byte OP in C
    inc     l
    ld      a, b
    and     [hl]    ; result of low byte OP in A
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], a
    ret

; AND2kr a b -- a b c
_AND2kr::
    RBIT_2K_SETUP
    jr      _AND2k.continue

; ORA a b -- c
_ORA::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    or      [hl]
    ld      [hl], a
    ret

; ORAr a b -- c
_ORAr::
    RST_HA_dec_ptr
    jr      _ORA.continue

; ORAk a b -- a b c
_ORAk::
    WST_HL_dec
    ld      a, [hld]
    or      [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; ORAkr a b -- a b c
_ORAkr::
    RST_HL_dec
    ld      a, [hld]
    or      [hl]
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; ORA2 a b -- c
_ORA2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hWSTPtr], a
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
    
; ORA2r a b -- c
_ORA2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hRSTPtr], a
    jr      _ORA2.continue

; ORA2k a b -- a b c
_ORA2k::
    WBIT_2K_SETUP
    ld      a, [hli]
    ld      b, [hl]
    inc     l
    or      [hl]
    ld      c, a    ; result of high byte OP in C
    inc     l
    ld      a, b
    or      [hl]    ; result of low byte OP in A
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], a
    ret

; ORA2kr a b -- a b c
_ORA2kr::
    RBIT_2K_SETUP
    jr      _ORA2k.continue

; EOR a b -- c
_EOR::
    WST_HA_dec_ptr
.continue
    ld      l, a
    ld      a, [hld]
    xor     [hl]
    ld      [hl], a
    ret

; EORr a b -- c
_EORr::
    RST_HA_dec_ptr
    jr      _EOR.continue

; EORk a b -- a b c
_EORk::
    WST_HL_dec
    ld      a, [hld]
    xor     [hl]
    inc     l
    inc     l
    ld      [hli], a
    WST_PTR_L
    ret

; EORkr a b -- a b c
_EORkr::
    RST_HL_dec
    ld      a, [hld]
    xor     [hl]
    inc     l
    inc     l
    ld      [hli], a
    RST_PTR_L
    ret

; EOR2 a b -- c
_EOR2::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hWSTPtr], a
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

; EOR2r a b -- c
_EOR2r::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    dec     a
    ld      l, a
    dec     a
    ldh     [hRSTPtr], a
    jr      _EOR2.continue

; EOR2k a b -- a b c
_EOR2k::
    WBIT_2K_SETUP
    ld      a, [hli]
    ld      b, [hl]
    inc     l
    xor     [hl]
    ld      c, a    ; result of high byte OP in C
    inc     l
    ld      a, b
    xor     [hl]    ; result of low byte OP in A
    inc     l
    ld      [hl], c
    inc     l
    ld      [hl], a
    ret

; EOR2kr a b -- a b c
_EOR2kr::
    RBIT_2K_SETUP
    jr      _EOR2k.continue

; SFT a shift8 -- c
_SFT::
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

; SFTr a shift8 -- c
_SFTr::
    RST_HA_dec_ptr
    jr      _SFT.continue

; SFTk a shift8 -- a shift8 c
_SFTk::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    ldh     [hWSTPtr], a
.continue
    dec     a
    dec     a
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
    inc     l   ; TODO: Optimize hl traversal
    inc     l
    ld      [hl], c
    ret

; SFTkr a shift8 -- a shift8 c
_SFTkr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    ldh     [hRSTPtr], a
    jr      _SFTk.continue


; SFT2 a shift8 -- c
_SFT2::
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

; SFT2r a shift8 -- c
_SFT2r::
    RST_HA_dec_ptr
    jr      _SFT2.continue
    
; SFT2k a shift8 -- a shift8 c
_SFT2k::
    ld      h, HIGH(wWST)
    ldh     a, [hWSTPtr]
    inc     a
    inc     a
    ldh     [hWSTPtr], a
.continue
    sub     5
    ld      l, a

    ld      b, [hl]
    inc     l
    ld      c, [hl]
    inc     l
    ld      d, [hl]
    inc     l

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

; SFT2kr a shift8 -- a shift8 c
_SFT2kr::
    ld      h, HIGH(wRST)
    ldh     a, [hRSTPtr]
    inc     a
    inc     a
    ldh     [hRSTPtr], a
    jr      _SFT2k.continue
