;
; CLI Devices
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

SECTION "UXNCLI WRAM", HRAM
hCursorAddr::       ds 2

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
    ldh     a, [hCursorAddr]
    ld      h, a
    ldh     a, [hCursorAddr+1]
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
    ldh     [hCursorAddr], a
    ld      a, l
    ldh     [hCursorAddr+1], a
.notWrite
    ret

; d = device
; bc = data
DevConsoleDEO2::
    ; TODO: write to console
    ret

DevNil::
    ret