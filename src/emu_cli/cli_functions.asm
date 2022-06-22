SECTION "UXNCLI Functions", ROM0

mode_init:
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

    ret
