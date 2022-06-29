SECTION "UXNCLI Functions", ROM0

ModeInit:
    ; Load console font
    ld      de, FontTiles
    ld      hl, $8000
    ld      bc, FontTiles.end - FontTiles
    call    LCDMemcpy

    ; Setup console cursor
    ld      a, HIGH($9800)
    ld      [wCursorAddr], a
    ld      a, LOW($9800)
    ld      [wCursorAddr+1], a

    ; Clear console
    ld      hl, $9800
    ld      bc, $0400
    xor     a
    call    LCDMemset

    ret
