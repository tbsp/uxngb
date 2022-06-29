
SECTION "Device Handlers", ROM0, ALIGN[7]
DeviceHandlers::
    dw DevSystemDEI, DevSystemDEI2, DevSystemDEO, DevSystemDEO2 ; system
    dw DevNil, DevNil, DevConsoleDEO, DevConsoleDEO2              ; console
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (file0)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (file1)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (datetime)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty

SECTION "UXNCLI Device Defaults", ROM0
DefaultDefaults::
    ds 256, 0

SECTION "UXNCLI WRAM", WRAM0
wCursorAddr::       ds 2

SECTION "Font Tiles", ROM0
FontTiles:
    incbin "res/comic8x8_linear.2bpp"
.end

SECTION "UXNCLI Vectors", ROM0

VectorHandlers::
    ret

SECTION "UXNCLI Devices", ROM0

DevSystemDEI::
    ret

DevSystemDEI2::
    ret

; d = device
; b = data
DevSystemDEO::
    ; TODO: "Special handling" (set wst/rst?)
    ret

; d = device
; bc = data
DevSystemDEO2::
    ret

; d = device
; b = data
DevConsoleDEO::
    ld      a, d
    cp      $18
    jr      nz, .notWrite

    ; write character
    ld      a, [wCursorAddr]
    ld      h, a
    ld      a, [wCursorAddr+1]
    ld      l, a

    ; special characters
    ld      a, b    ; value of char
    cp      $0a     ; newline
    jr      nz, :+
    ld      a, l
    and     %11100000   ; mask off fractional portion
    ld      l, a
    ld      de, $0020
    add     hl, de
    jr      .storeCursor
:

    sub     $20     ; ASCII offset
    ld      c, 1    ; char count
    call    LCDMemsetSmall

.storeCursor
    ld      a, h
    ld      [wCursorAddr], a
    ld      a, l
    ld      [wCursorAddr+1], a
.notWrite
    ret

; d = device
; bc = data
DevConsoleDEO2::
    ; TODO: write to console
    ret

DevNil::
    ret