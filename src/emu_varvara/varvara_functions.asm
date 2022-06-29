;
; Varvara functions
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

include "defines.asm"

SECTION "Varvara Functions", ROM0

ModeInit:
    ; Zero all tiles (with the screen on, just to be super inefficient)
    ld      hl, $8000
    ld      bc, $9800 - $8000
    xor     a
    call    LCDMemset

    ld      [wOAMIndex], a

    ; Clear foreground object source address table
    ld      hl, wObjSourceAddrs
    ld      c, wObjSourceAddrs.end - wObjSourceAddrs
    rst     MemsetSmall

    ; Setup static tilemap for background 'layer'
    ld      hl, $9800
    ld      de, SCRN_VX_B - SCRN_X_B
    xor     a
    ld      c, 12
    call    TilemapIncFill

    ; second chunk (after scanline tile bank swap)
    xor     a
    ld      c, SCRN_Y_B - 12
    call    TilemapIncFill

    ; setup mid-screen tile bank swap interrupt
    ld	    a, STATF_LYC
    ldh	    [rSTAT], a
    ld      a, 96       ; switch background tiles on line 96
    ldh     [rLYC], a

    ; Clear shadow OAM for foreground 'layer'
    ld      hl, wShadowOAM
    ld      c, $A0
    xor     a
    rst     MemsetSmall

    ret

TilemapIncFill:
.y
    ld      b, 10
.x
    push    af
    wait_vram
    pop     af
    ld      [hli], a    ; two safe bytes per STAT check
    inc     a
    ld      [hli], a
    inc     a
    dec     b
    jr      nz, .x
    add     hl, de
    dec     c
    jr      nz, .y
    ret

