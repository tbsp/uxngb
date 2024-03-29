
INCLUDE "defines.asm"


SECTION "Header", ROM0[$100]

    ; This is your ROM's entry point
    ; You have 4 bytes of code to do... something
    sub     $11 ; This helps check if we're on CGB more efficiently
    jr      EntryPoint

    ; Make sure to allocate some space for the header, so no important
    ; code gets put there and later overwritten by RGBFIX.
    ; RGBFIX is designed to operate over a zero-filled header, so make
    ; sure to put zeros regardless of the padding value. (This feature
    ; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
    ; introduced in that version.)
    ds      $150 - @, 0

EntryPoint:
    ldh     [hConsoleType], a

Reset::
    di      ; Disable interrupts while we set up

    ; Kill sound
    xor     a
    ldh     [rNR52], a

    ; Wait for VBlank and turn LCD off
.waitVBlank
    ldh     a, [rLY]
    cp      SCRN_Y
    jr      c, .waitVBlank
    xor     a
    ldh     [rLCDC], a
    ; Goal now: set up the minimum required to turn the LCD on again
    ; A big chunk of it is to make sure the VBlank handler doesn't crash

    ld      sp, wStackBottom

    ld      hl, OAMDMA
    lb      bc, OAMDMA.end - OAMDMA, LOW(hOAMDMA)
.copyOAMDMA
    ld      a, [hli]
    ldh     [c], a
    inc     c
    dec     b
    jr      nz, .copyOAMDMA

    ldh     a, [hConsoleType]
    or      a
    jr      z, .cgb

    ; Set Palettes
    ld      a, %00011011
    ldh     [rBGP], a
    ldh     [rOBP0], a
    ldh     [rOBP1], a

    jr      .doneConsoleCheck
.cgb
    ; Enable double-speed mode
    ; Note: hello-pong gets a stray missing tile in the background when double-speed mode is enabled
    ;  -> Without double speed mode there's a single missing line instead (also seen in BGB), but only in CGB mode
    ld      a, KEY1F_PREPARE
    ldh     [rKEY1], a
    stop

.doneConsoleCheck

    ; You will also need to reset your handlers' variables below
    ; I recommend reading through, understanding, and customizing this file
    ; in its entirety anyways. This whole file is the "global" game init,
    ; so it's strongly tied to your own game.
    ; I don't recommend clearing large amounts of RAM, nor to init things
    ; here that can be initialized later.

    ; Reset variables necessary for the VBlank handler to function correctly
    ; But only those for now
    xor     a
    ldh     [hVBlankFlag], a
    ldh     [hOAMHigh], a
    ldh     [hCanSoftReset], a
    ldh     [hPalettePending], a
    dec     a ; ld a, $FF
    ldh     [hHeldKeys], a
    ldh     [hPriorKeys], a

    ; Initialize frame counter (counts down 60 frames to measure a ~second)
    ld      a, 60
    ldh     [hFrameCounter], a

    ; Select wanted interrupts here
    ; You can also enable them later if you want
    ld      a, IEF_VBLANK | IEF_LCDC
    ldh     [rIE], a
    xor     a
    ei      ; Only takes effect after the following instruction
    ldh     [rIF], a ; Clears "accumulated" interrupts

    ; Init shadow regs
    ; xor a
    ldh     [hSCY], a
    ldh     [hSCX], a
    ld      a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000 | LCDCF_BG9800 | LCDCF_OBJON | LCDCF_OBJ8
    ldh     [hLCDC], a
    ; And turn the LCD on!
    ldh     [rLCDC], a

    ; Clear OAM, so it doesn't display garbage
    ; This will get committed to hardware OAM after the end of the first
    ; frame, but the hardware doesn't display it, so that's fine.
    ld      hl, wShadowOAM
    ld      c, NB_SPRITES * 4
    xor     a
    rst     MemsetSmall
    ld      a, h ; ld a, HIGH(wShadowOAM)
    ldh     [hOAMHigh], a

    ; `Intro`'s bank has already been loaded earlier
    jp      Intro

SECTION "OAM DMA routine", ROM0

; OAM DMA prevents access to most memory, but never HRAM.
; This routine starts an OAM DMA transfer, then waits for it to complete.
; It gets copied to HRAM and is called there from the VBlank handler
OAMDMA:
    ldh     [rDMA], a
    ld      a, NB_SPRITES
.wait
    dec     a
    jr      nz, .wait
    ret
.end

SECTION "Global vars", HRAM

; 0 if CGB (including DMG mode and GBA), non-zero for other models
hConsoleType:: db

SECTION "OAM DMA", HRAM

hOAMDMA::
    ds OAMDMA.end - OAMDMA


; Manually positioned to maximize WRAM space for Uxn
SECTION UNION "Shadow OAM", WRAM0[$E000 - 1024]

wShadowOAM::
    ds NB_SPRITES * 4
wOAMIndex::         ; Index of next free entry in OAM for dynamically generated objects
    ds 1
wOAMEndIndex::      ; Index of last enty used in the previous frame
    ds 1

; This ensures that the stack is packed into a safe portion of WRAM
SECTION "Stack", WRAM0[$E000 - 512 - 256 - STACK_SIZE]

    ds STACK_SIZE
wStackBottom:

