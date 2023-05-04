;
; Core Emulator for uxnemu
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

DEF MODE_CLI        EQU $00 ; mode with console input/output devices
DEF MODE_VARVARA    EQU $01 ; mode with varvara compliant devices (very slow Screen due to lack of bitmap graphical hardware support)
DEF MODE_TILE       EQU $02 ; mode with tile-based devices for improved performance on tile-based hardware (notional)

DEF MODE            EQU MODE_VARVARA

; Include different files based on active mode
IF MODE == MODE_CLI
    include "emu_cli/cli_devices.asm"
    include "emu_cli/cli_functions.asm"
ELIF MODE == MODE_VARVARA
    include "emu_varvara/varvara_devices.asm"
    include "emu_varvara/varvara_functions.asm"
ELSE

ENDC

SECTION "UXN HRAM", HRAM
hPC::               ds 2
hWSTPtr::           ds 1    ; should exist as the second last byte in the wst (programs that depend on that will fail right now)
hRSTPtr::           ds 1    ; should exist as the second last byte in the rst (programs that depend on that will fail right now)
hPendingPalettes::  ds 16   ; one full BG and OBJ palette for CGB
hFrameCounter::     ds 1    ; used to increment datetime seconds every 60 frames

; The stacks/devices are explicitly stored at the end of WRAM to allow UXN memory to overflow from SRAM to WRAM, which
;  allows UXN to access 15616 (16384-512-256) bytes of RAM (instead of just 8192 bytes).
SECTION "UXN Stacks", WRAM0[$E000-512]
wWST::      ds 256
wRST::      ds 256

SECTION "UXN Devices", WRAM0[$E000-512-256]
wDevices::  ds 16*16

SECTION "UXN Memory", SRAM[$A000]
eUXNMemory::
eZeroPage::     ds 256
ePageProgram::

SECTION "Intro", ROM0

Intro::
    ; call mode-specific initialization
    call    ModeInit

    ; Initialize emulator state

    ; enable external RAM to hold UXN 64KB memory
    xor     a
    ld      [$4000], a  ; set to ram bank 0
    ld      a, $0A
    ld      [$00], a    ; enable SRAM access

    ; Initialize device memory
    ld      de, DefaultDefaults
    ld      hl, wDevices
    ld      c, 0
    rst     MemcpySmall

    ; uxn_boot
    ; initialize the single bank of external RAM we're currently using
    ; TODO: If we expand to support the full 64KB UXN memory space, clear all 8 banks
    xor     a
    ld      hl, eUXNMemory
    ld      bc, $2000
    rst     Memset

    ; Initialize stack pointers
    ldh     [hWSTPtr], a
    ldh     [hRSTPtr], a

    ; Copy entire ROM external RAM
    ; TODO: Copy in banks
    ld      de, staticROM
    ld      hl, ePageProgram
    ld      bc, $3A00-$100 ; Blindly copy the 0x3A00 maximum supported ROM size minus the zero page
    call    Memcpy

    ; initialize PC
    ld      a, HIGH(ePageProgram)
    ldh     [hPC], a
    ld      a, LOW(ePageProgram)
    ldh     [hPC+1], a

    ; initial eval loop
    call    uxn_eval

brk_loop:
    ; BRK idles, calling vector handlers as required once per VBlank
    rst     WaitVBlank
    call    VectorHandlers
    jr      brk_loop

system_halt:
    rst     WaitVBlank
    jr      system_halt


uxn_eval:

    ; TODO: check shutdown vector (dev:$0f)
    ; TODO: check hWSTPtr

    ; Thanks jvsTSX! Saved one cycle!
    ld      hl, hPC     ; get current PC
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a

    ld      a, [bc]     ; read next instruction
    inc     bc          ; increment PC

    ld      [hl], c     ; save updated PC
    dec     l
    ld      [hl], b

    ; obtain instruction handler address
    ld      b, HIGH(instr_jump_table)
    add     a
    jr      nc, :+
    inc     b
:   ld      c, a

    ld      h, b
    ld      l, c
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL      ; call handler

    jr      uxn_eval


SECTION "Instruction Jump Table", ROM0, ALIGN[8] ; Aligned to speed up instruction dispatch
instr_jump_table:
    ;   0x00     0x01     0x02     0x03     0x04     0x05     0x06     0x07     0x08     0x09     0x0A     0x0B     0x0C     0x0D     0x0E     0x0F
    ; 0x00
    dw _BRK,    _INC,    _POP,    _NIP,    _SWP,    _ROT,    _DUP,    _OVR,    _EQU,    _NEQ,    _GTH,    _LTH,    _JMP,    _JCN,    _JSR,    _STH
    ; 0x10
    dw _LDZ,    _STZ,    _LDR,    _STR,    _LDA,    _STA,    _DEI,    _DEO,    _ADD,    _SUB,    _MUL,    _DIV,    _AND,    _ORA,    _EOR,    _SFT
    ; 0x20
    dw _NIL,    _INC2,   _POP2,   _NIP2,   _SWP2,   _ROT2,   _DUP2,   _OVR2,   _EQU2,   _NEQ2,   _GTH2,   _LTH2,   _JMP2,   _JCN2,   _JSR2,   _STH2
    ; 0x30
    dw _LDZ2,   _STZ2,   _LDR2,   _STR2,   _LDA2,   _STA2,   _DEI2,   _DEO2,   _ADD2,   _SUB2,   _MUL2,   _DIV2,   _AND2,   _ORA2,   _EOR2,   _SFT2
    ; 0x40
    dw _NIL,    _INCr,   _POPr,   _NIPr,   _SWPr,   _ROTr,   _DUPr,   _OVRr,   _EQUr,   _NEQr,   _GTHr,   _LTHr,   _JMPr,   _JCNr,   _JSRr,   _STHr
    ; 0x50
    dw _LDZr,   _STZr,   _LDRr,   _STRr,   _LDAr,   _STAr,   _DEIr,   _DEOr,   _ADDr,   _SUBr,   _MULr,   _DIVr,   _ANDr,   _ORAr,   _EORr,   _SFTr
    ; 0x60
    dw _NIL,    _INC2r,  _POP2r,  _NIP2r,  _SWP2r,  _ROT2r,  _DUP2r,  _OVR2r,  _EQU2r,  _NEQ2r,  _GTH2r,  _LTH2r,  _JMP2r,  _JCN2r,  _JSR2r,  _STH2r
    ; 0x70
    dw _LDZ2r,  _STZ2r,  _LDR2r,  _STR2r,  _LDA2r,  _STA2r,  _DEI2r,  _DEO2r,  _ADD2r,  _SUB2r,  _MUL2r,  _DIV2r,  _AND2r,  _ORA2r,  _EOR2r,  _SFT2r
    ; 0x80
    dw _LIT,    _INCk,   _POPk,   _NIPk,   _SWPk,   _ROTk,   _DUPk,   _OVRk,   _EQUk,   _NEQk,   _GTHk,   _LTHk,   _JMPk,   _JCNk,   _JSRk,   _STHk
    ; 0x90
    dw _LDZk,   _STZk,   _LDRk,   _STRk,   _LDAk,   _STAk,   _DEIk,   _DEOk,   _ADDk,   _SUBk,   _MULk,   _DIVk,   _ANDk,   _ORAk,   _EORk,   _SFTk
    ; 0xA0
    dw _LIT2,   _INC2k,  _POP2k,  _NIP2k,  _SWP2k,  _ROT2k,  _DUP2k,  _OVR2k,  _EQU2k,  _NEQ2k,  _GTH2k,  _LTH2k,  _JMP2k,  _JCN2k,  _JSR2k,  _STH2k
    ; 0xB0
    dw _LDZ2k,  _STZ2k,  _LDR2k,  _STR2k,  _LDA2k,  _STA2k,  _DEI2k,  _DEO2k,  _ADD2k,  _SUB2k,  _MUL2k,  _DIV2k,  _AND2k,  _ORA2k,  _EOR2k,  _SFT2k
    ; 0xC0
    dw _LITr,   _INCkr,  _POPkr,  _NIPkr,  _SWPkr,  _ROTkr,  _DUPkr,  _OVRkr,  _EQUkr,  _NEQkr,  _GTHkr,  _LTHkr,  _JMPkr,  _JCNkr,  _JSRkr,  _STHkr
    ; 0xD0
    dw _LDZkr,  _STZkr,  _LDRkr,  _STRkr,  _LDAkr,  _STAkr,  _DEIkr,  _DEOkr,  _ADDkr,  _SUBkr,  _MULkr,  _DIVkr,  _ANDkr,  _ORAkr,  _EORkr,  _SFTkr
    ; 0xE0
    dw _LIT2r,  _INC2kr, _POP2kr, _NIP2kr, _SWP2kr, _ROT2kr, _DUP2kr, _OVR2kr, _EQU2kr, _NEQ2kr, _GTH2kr, _LTH2kr, _JMP2kr, _JCN2kr, _JSR2kr, _STH2kr
    ; 0xF0
    dw _LDZ2kr, _STZ2kr, _LDR2kr, _STR2kr, _LDA2kr, _STA2kr, _DEI2kr, _DEO2kr, _ADD2kr, _SUB2kr, _MUL2kr, _DIV2kr, _AND2kr, _ORA2kr, _EOR2kr, _SFT2kr

; Single byte so the output ROM runs right up to this point
SECTION "End Pad", ROM0[$3FFF]
    db $FF

; Attachment point for appended UXN ROM
SECTION "UXN ROM", ROMX[$4000]
staticROM:
