;
; CLI functions
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


SECTION "UXNCLI Functions", ROM0

ModeInit:
    ; Load console font
    ld      de, FontTiles
    ld      hl, $8000
    ld      bc, FontTiles.end - FontTiles
    call    LCDMemcpy

    ; Setup console cursor
    ld      a, HIGH($9800)
    ldh     [hCursorAddr], a
    ld      a, LOW($9800)
    ldh     [hCursorAddr+1], a

    ; Clear console
    ld      hl, $9800
    ld      bc, $0400
    xor     a
    call    LCDMemset

    ret
