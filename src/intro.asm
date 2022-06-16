DEF PageProgram EQU $0100

SECTION "Font Tiles", ROMX
FontTiles:
    ;incbin "res/chicago8x8.2bpp"
    incbin "res/comic8x8_linear.2bpp"
.end

SECTION "UXN HRAM", HRAM
pc::        ds 2
wst_ptr::   ds 1    ; should exist as the second last byte in the wst (programs that depend on that will fail right now)
rst_ptr::   ds 1    ; should exist as the second last byte in the rst (programs that depend on that will fail right now)

; The default stacks "live" at $10000-$100ff and $10100-$101ff in UXN memory
SECTION "UXN Stacks", WRAM0, ALIGN[0]
w_st::      ds 256
r_st::      ds 256

SECTION "UXN Devices", WRAM0, ALIGN[0]
devices::   ds 16*16

SECTION "UXN Memory", SRAM[$A000]
uxn_memory::
zero_page::     ds 256
page_program::

SECTION "Console WRAM", WRAM0
cursor_addr::   ds 2

SECTION "Intro", ROMX

Intro::
    ; Load console font
    ld      de, FontTiles
    ld      hl, $8000
    ld      bc, FontTiles.end - FontTiles
    call    LCDMemcpy

    ; Setup console cursor
    ld      a, HIGH($9800)
    ld      [cursor_addr], a
    ld      a, LOW($9800)
    ld      [cursor_addr+1], a

    ; Clear console
    ld      hl, $9800
    ld      bc, $0400
    xor     a
    call    LCDMemset

    ; Initialize emulator state

    ; enable external RAM to hold UXN 64KB memory
    xor     a
    ld      [$4000], a  ; set to ram bank 0
    ld      a, $0A
    ld      [$00], a    ; enable SRAM access

    ; TODO: initialize device memory, likely copy from LUT (screen size, wst/rst, etc)

    ; uxn_boot
    ; initialize all 8 banks of RAM with 0
    ld      d, 8
    .zeroRAM
        xor     a
        ld      hl, $A000
        ld      bc, $1FFF
        push    de
        rst     Memset
        pop     de
        dec     d
        jr      nz, .zeroRAM

    ; Initialize stack pointers
    ldh     [wst_ptr], a
    ldh     [rst_ptr], a

    ; Copy entire ROM external RAM
    ; TODO: Copy in banks
    ld      de, staticROM
    ld      hl, page_program
    ld      bc, staticROM.end - staticROM
    call    Memcpy

    ; initialize PC
    ld      a, HIGH(page_program)
    ldh     [pc], a
    ld      a, LOW(page_program)
    ldh     [pc+1], a

    ; fall through to uxn_eval

uxn_eval:

    ; TODO: check shutdown vector (dev:$0f)
    ; TODO: check wst_ptr

    ldh     a, [pc]
    ld      h, a
    ldh     a, [pc+1]
    ld      l, a
    ld      b, [hl]     ; read next instruction

    ld      a, b        ; see if we've hit a BRK
    or      a
    jr      z, brk_loop

    inc     hl          ; increment PC, and store new value
    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a

    ld      a, b
    ld      b, 0
    sla     a           ; get jump table offset for instruction handler
    ld      c, a
    rl      b
    ld      hl, instr_jump_table
    add     hl, bc
    ld      a, [hli]
    ld      h, [hl]
    ld      l, a
    rst     CallHL      ; call handler

    jr      uxn_eval

brk_loop:
    ; BRK idles, calling vector handlers as required
    rst     WaitVBlank

    ; TODO: controller vector
    ; TODO: screen vector

    jr      brk_loop


SECTION "Device Handlers", ROM0, ALIGN[7]
device_handlers::
    dw dev_system_dei, dev_system_dei2, dev_system_deo, dev_system_deo2 ; system
    dw dev_nil, dev_nil, dev_console_deo, dev_console_deo2              ; console
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file0)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file1)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (datetime)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty

SECTION "Instruction Jump Table", ROM0
instr_jump_table:
    ; 0x00
    dw instr_BRK
    dw instr_INC
    dw instr_POP
    dw instr_NIP
    dw instr_SWP
    dw instr_ROT
    dw instr_DUP
    dw instr_OVR
    dw instr_EQU
    dw instr_NEQ
    dw instr_GTH
    dw instr_LTH
    dw instr_JMP
    dw instr_JCN
    dw instr_JSR
    dw instr_STH
    dw instr_LDZ
    dw instr_STZ
    dw instr_LDR
    dw instr_STR
    dw instr_LDA
    dw instr_STA
    dw instr_DEI
    dw instr_DEO
    dw instr_ADD
    dw instr_SUB
    dw instr_MUL
    dw instr_DIV
    dw instr_AND
    dw instr_ORA
    dw instr_EOR
    dw instr_SFT
    ; 0x20
    dw instr_NIL
    dw instr_INC2
    dw instr_POP2
    dw instr_NIP2
    dw instr_SWP2
    dw instr_ROT2
    dw instr_DUP2
    dw instr_OVR2
    dw instr_EQU2
    dw instr_NEQ2
    dw instr_GTH2
    dw instr_LTH2
    dw instr_JMP2
    dw instr_JCN2
    dw instr_JSR2
    dw instr_STH2
    dw instr_LDZ2
    dw instr_STZ2
    dw instr_LDR2
    dw instr_STR2
    dw instr_LDA2
    dw instr_STA2
    dw instr_DEI2
    dw instr_DEO2
    dw instr_ADD2
    dw instr_SUB2
    dw instr_MUL2
    dw instr_DIV2
    dw instr_AND2
    dw instr_ORA2
    dw instr_EOR2
    dw instr_SFT2
    ; 0x40
    dw instr_NIL
    dw instr_INCr
    dw instr_POPr
    dw instr_NIPr
    dw instr_SWPr
    dw instr_ROTr
    dw instr_DUPr
    dw instr_OVRr
    dw instr_EQUr
    dw instr_NEQr
    dw instr_GTHr
    dw instr_LTHr
    dw instr_JMPr
    dw instr_JCNr
    dw instr_JSRr
    dw instr_STHr
    dw instr_LDZr
    dw instr_STZr
    dw instr_LDRr
    dw instr_STRr
    dw instr_LDAr
    dw instr_STAr
    dw instr_DEIr
    dw instr_DEOr
    dw instr_ADDr
    dw instr_SUBr
    dw instr_MULr
    dw instr_DIVr
    dw instr_ANDr
    dw instr_ORAr
    dw instr_EORr
    dw instr_SFTr
    ; 0x60
    dw instr_NIL
    dw instr_INC2r
    dw instr_POP2r
    dw instr_NIP2r
    dw instr_SWP2r
    dw instr_ROT2r
    dw instr_DUP2r
    dw instr_OVR2r
    dw instr_EQU2r
    dw instr_NEQ2r
    dw instr_GTH2r
    dw instr_LTH2r
    dw instr_JMP2r
    dw instr_JCN2r
    dw instr_JSR2r
    dw instr_STH2r
    dw instr_LDZ2r
    dw instr_STZ2r
    dw instr_LDR2r
    dw instr_STR2r
    dw instr_LDA2r
    dw instr_STA2r
    dw instr_DEI2r
    dw instr_DEO2r
    dw instr_ADD2r
    dw instr_SUB2r
    dw instr_MUL2r
    dw instr_DIV2r
    dw instr_AND2r
    dw instr_ORA2r
    dw instr_EOR2r
    dw instr_SFT2r
    ; 0x80
    dw instr_LIT
    dw instr_INCk
    dw instr_POPk
    dw instr_NIPk
    dw instr_SWPk
    dw instr_ROTk
    dw instr_DUPk
    dw instr_OVRk
    dw instr_EQUk
    dw instr_NEQk
    dw instr_GTHk
    dw instr_LTHk
    dw instr_JMPk
    dw instr_JCNk
    dw instr_JSRk
    dw instr_STHk
    dw instr_LDZk
    dw instr_STZk
    dw instr_LDRk
    dw instr_STRk
    dw instr_LDAk
    dw instr_STAk
    dw instr_DEIk
    dw instr_DEOk
    dw instr_ADDk
    dw instr_SUBk
    dw instr_MULk
    dw instr_DIVk
    dw instr_ANDk
    dw instr_ORAk
    dw instr_EORk
    dw instr_SFTk
    ; 0xA0
    dw instr_LIT2
    dw instr_INC2k
    dw instr_POP2k
    dw instr_NIP2k
    dw instr_SWP2k
    dw instr_ROT2k
    dw instr_DUP2k
    dw instr_OVR2k
    dw instr_EQU2k
    dw instr_NEQ2k
    dw instr_GTH2k
    dw instr_LTH2k
    dw instr_JMP2k
    dw instr_JCN2k
    dw instr_JSR2k
    dw instr_STH2k
    dw instr_LDZ2k
    dw instr_STZ2k
    dw instr_LDR2k
    dw instr_STR2k
    dw instr_LDA2k
    dw instr_STA2k
    dw instr_DEI2k
    dw instr_DEO2k
    dw instr_ADD2k
    dw instr_SUB2k
    dw instr_MUL2k
    dw instr_DIV2k
    dw instr_AND2k
    dw instr_ORA2k
    dw instr_EOR2k
    dw instr_SFT2k
    ; 0xC0
    dw instr_LITr
    dw instr_INCkr
    dw instr_POPkr
    dw instr_NIPkr
    dw instr_SWPkr
    dw instr_ROTkr
    dw instr_DUPkr
    dw instr_OVRkr
    dw instr_EQUkr
    dw instr_NEQkr
    dw instr_GTHkr
    dw instr_LTHkr
    dw instr_JMPkr
    dw instr_JCNkr
    dw instr_JSRkr
    dw instr_STHkr
    dw instr_LDZkr
    dw instr_STZkr
    dw instr_LDRkr
    dw instr_STRkr
    dw instr_LDAkr
    dw instr_STAkr
    dw instr_DEIkr
    dw instr_DEOkr
    dw instr_ADDkr
    dw instr_SUBkr
    dw instr_MULkr
    dw instr_DIVkr
    dw instr_ANDkr
    dw instr_ORAkr
    dw instr_EORkr
    dw instr_SFTkr
    ; 0xE0
    dw instr_LIT2r
    dw instr_INC2kr
    dw instr_POP2kr
    dw instr_NIP2kr
    dw instr_SWP2kr
    dw instr_ROT2kr
    dw instr_DUP2kr
    dw instr_OVR2kr
    dw instr_EQU2kr
    dw instr_NEQ2kr
    dw instr_GTH2kr
    dw instr_LTH2kr
    dw instr_JMP2kr
    dw instr_JCN2kr
    dw instr_JSR2kr
    dw instr_STH2kr
    dw instr_LDZ2kr
    dw instr_STZ2kr
    dw instr_LDR2kr
    dw instr_STR2kr
    dw instr_LDA2kr
    dw instr_STA2kr
    dw instr_DEI2kr
    dw instr_DEO2kr
    dw instr_ADD2kr
    dw instr_SUB2kr
    dw instr_MUL2kr
    dw instr_DIV2kr
    dw instr_AND2kr
    dw instr_ORA2kr
    dw instr_EOR2kr
    dw instr_SFT2kr

SECTION "UXN ROM", ROMX, ALIGN[$0100]
staticROM:
    ;incbin "res/tests.rom"
    ;incbin "res/console.rom"
    incbin "res/tests_noStacks.rom"
.end