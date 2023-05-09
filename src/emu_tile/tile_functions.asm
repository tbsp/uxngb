
include "defines.asm"

SECTION "Tile Functions", ROM0

ModeInit:
    ; Zero all tiles + tilemap (with the screen on, just to be super inefficient)
    ld      hl, $8000
    ld      bc, $9c00 - $8000
    xor     a
    call    LCDMemset

    ld      [wOAMIndex], a

    ; Clear shadow OAM
    ld      hl, wShadowOAM
    ld      c, $a0
    xor     a
    rst     MemsetSmall

    ret
