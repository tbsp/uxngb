;
; Tile Devices
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

; I've attempted a rough RGB->Intensity conversion, tuned to values
;  which look good for screen.tal, but in other cases the result is not
;  very desireable, so for now it's disabled. I'm not sure if the solution
;  is just a different table of values, or an entirely different approach.
DEF ENABLE_DMG_PALETTES     EQU 0

SECTION "Device Handlers", ROM0, ALIGN[7]
DeviceHandlers::
    dw DevSystemDEI, DevSystemDEI2, DevSystemDEO, DevSystemDEO2 ; system
    dw DevNil, DevNil, DevConsoleDEO, DevConsoleDEO2            ; console
    dw DevTileDEI, DevTileDEI2, DevTileDEO, DevTileDEO2         ; tile
    dw DevNil, DevNil, DevNil, DevNil                           ; audio
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; controller
    dw DevNil, DevNil, DevNil, DevNil                           ; mouse
    dw DevNil, DevNil, DevNil, DevNil                           ; empty (file0)
    dw DevNil, DevNil, DevNil, DevNil                           ; empty (file1)
    dw DevNil, DevNil, DevNil, DevNil                           ; empty (datetime)
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; empty
    dw DevNil, DevNil, DevNil, DevNil                           ; empty

SECTION "Tile Device Defaults", ROM0
DeviceDefaults::
    ; system (0x00)
    dw 0        ; vector
    db 0        ; wst
    db 0        ; rst
    ds 4, 0     ; pad
    dw 0        ; red
    dw 0        ; blue
    dw 0        ; green
    db 0        ; debug
    db 0        ; state
    ; console (0x10)
    ds 16, 0

    ; tile (0x20)
    ; Note: Unfinished
    dw 0        ; vector
    db 20       ; screen tile width (0-255 tiles)
    db 18       ; screen tile height (0-255 tiles)
    db 0        ; x (0-255)
    db 0        ; y (0-255)
    dw 0        ; addr (for tile/tilemap copy source)
    db 0        ; auto (autoY, autoX, autoSrc, 5bit length (32 max))
                ; 76543210
                ; |||+++++- length
                ; ||+------ autoSrc
                ; |+------- autoX
                ; +-------- autoY
    db 0        ; repeat (number of times to automatically repeat write operations)
    db 0        ; tileID (tileID for tilemap/sprite, target tile for tile copies)
    db 0        ; write operation
                ;  - 00 tiles: autoBlend, mode, blending [4] TODO: Remove blending, add bit to set BG or sprite tiles target?
                ;    76543210
                ;    ||||++++- blending
                ;    |||+----- mode (1bpp/2bpp)
                ;    ||+------ autoBlend
                ;    ++------- operation
                ;  - 01 tilemap: ?????, src -> Set tilemap based on tileID or copy from add
                ;    76543210
                ;    |||||||+- source (0: tileID, 1: addr)
                ;    ||+++++-- unused
                ;    ++------- operation
                ;  - 10 sprite: ???, 2x, flipY, flipX -> Create sprite at x/y/tileID with given flip flags
                ;    |||||||+- flipX
                ;    ||||||+-- flipY
                ;    ||++++--- unused
                ;    ++------- operation
                ;  - 11 unused
    db 0        ; scrollY (0-255)
    db 0        ; scrollX (0-255)
    db 0        ; unused
    db 0        ; unused

    ; audio
    ds 16, 0
    ; the rest
    ds 192, 0

SECTION "Tile WRAM", WRAM0[$DA00]
wPixelBlend:    ds 4    ; 4 blend bytes for the current blend mode

SECTION "Tile HRAM", HRAM
hDeviceByte:        ds 1    ; copy of the device byte for fast access
hDataByte:          ds 1    ; copy of the data byte for fast access
;hWorkingBytes:      ds 2    ; stores both the input bytes (initially) and the final pixel data (after blending)

SECTION UNION "Tile Scratch HRAM", HRAM
hX:                 ds 1
hY:                 ds 1
hDX:                ds 1
hDY:                ds 1
hAutoSrc:           ds 1

SECTION "Tile Vectors", ROM0

VectorHandlers::

    ; controller vector
    ld      hl, wDevices + $80
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a
    or      c
    jr      z, .noControllerVector

    ; Check for change in controller state
    ldh     a, [hPriorKeys]
    ld      d, a
    ldh     a, [hHeldKeys]
    cp      d
    jr      z, .noControllerVector
    ldh     [hPriorKeys], a

    ld      hl, eUXNMemory
    add     hl, bc

    ld      a, h
    ldh     [hPC], a
    ld      a, l
    ldh     [hPC+1], a

    call    uxn_eval    ; eval instructions until a BRK is hit
.noControllerVector

    ; screen vector
    ld      hl, wDevices + $20
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a
    or      c
    jr      z, .noScreenVector

    ld      hl, eUXNMemory
    add     hl, bc

    ld      a, h
    ldh     [hPC], a
    ld      a, l
    ldh     [hPC+1], a

    call    uxn_eval    ; eval instructions until a BRK is hit

    ; There's a decent chance the screen vector performed OAM updates, so
    ;  queue up an OAM DMA once all updates have been performed (to prevent
    ;  tearing in the likely case updates span more than one vblank).
    ld      a, HIGH(wShadowOAM)
    ldh     [hOAMHigh], a

.noScreenVector

    ret


SECTION "Tile Devices", ROM0

DevSystemDEI::
    ret

DevSystemDEI2::
    ret

; d = device
; b = data
DevSystemDEO::
    ret

IF ENABLE_DMG_PALETTES
    include "res/dmg_palette_lookup_generated.asm"
ENDC

; Convert the color values stored in the system device to a host-compatible palette
;  and then queue up a palette update for the next VBlank
UpdatePalette:

    ldh     a, [hConsoleType]
    or      a
    jr      z, .gbc

IF ENABLE_DMG_PALETTES
    ; For DMG convert the RGB to a 2 bit value
    ; Input: 4bit RGB for each of 4 channels, stored in 6 bytes
    ; Output: 2bit brightness value for each of 4 channels
    ld      hl, wDevices + $08 + 1
    call    ConvertTwoShades
    call    ConvertTwoShades
    
    ld      a, c
    ldh     [rBGP], a
    ldh     [rOBP0], a
ENDC

    ret
.gbc

    ; For CGB convert from 12 bit to 15 bit RGB
    ; CGB: xBBBBBGG_GGGRRRRR
    ld      hl, wDevices + $08
    ld      bc, hPendingPalettes
    call    ConvertTwoColors
    call    ConvertTwoColors

    ld      a, 1
    ldh     [hPalettePending], a
.done
    ret

IF ENABLE_DMG_PALETTES
ConvertTwoShades:
    ; color 0
    ld      a, [hli]    ; red
    inc     l
    and     %00001100
    add     a
    add     a
    ld      b, a
    ld      a, [hli]    ; green
    inc     l
    and     %00001100
    or      b
    ld      b, a
    ld      a, [hld]    ; blue
    dec     l
    dec     l
    dec     l
    and     %00001100
    srl     a
    srl     a
    or      b
    ; Now we have this color in the form: %00rrggbb
    push    hl
    ld      hl, DMGPaletteLookup
    add     l       ; offset to table value for this RGB value
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hl]
    pop     hl

    rrca            ; shift intensity bits into final palette in C
    rl      c
    rrca
    rl      c

    ; color 1
    ld      a, [hli]    ; red
    inc     l
    and     %11000000
    ld      b, a
    ld      a, [hli]    ; green
    inc     l
    and     %11000000
    srl     a
    srl     a
    or      b
    ld      b, a
    ld      a, [hld]    ; blue
    dec     l
    dec     l
    dec     l
    dec     l
    and     %11000000
    swap    a
    or      b
    srl     a
    srl     a
    ; Now we have this color in the form: %00rrggbb
    push    hl
    ld      hl, DMGPaletteLookup
    add     l       ; offset to table value for this RGB value
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hl]
    pop     hl

    rrca            ; shift intensity bits into final palette in C
    rl      c
    rrca
    rl      c

    ret
ENDC

ConvertTwoColors:
    ; color 0
    ld      a, [hli]    ; red
    inc     l
    and     $f0
    swap    a
    add     a
    ld      e, a
    ld      a, [hli]    ; green
    inc     l
    and     $f0
    ld      d, a
    add     a
    or      e
    ld      [bc], a     ; low byte of color 0
    inc     bc
    ld      a, d
    swap    a
    sra     a
    sra     a
    ld      d, a
    ld      a, [hld]    ; blue
    dec     l
    dec     l
    dec     l
    and     $f0
    sra     a
    or      d
    ld      [bc], a     ; high byte of color 0
    inc     bc

    ; color 1
    ld      a, [hli]    ; red
    inc     l
    and     $0f
    add     a
    ld      e, a
    ld      a, [hli]    ; green
    inc     l
    and     $0f
    ld      d, a
    swap    a
    add     a
    or      e
    ld      [bc], a     ; low byte of color 1
    inc     bc
    ld      a, d
    sra     a
    sra     a
    ld      d, a
    ld      a, [hld]    ; blue
    dec     l
    dec     l
    and     $0f
    swap    a
    sra     a
    or      d
    ld      [bc], a     ; high byte of color 1
    inc     bc

    ret

; d = device
; bc = data
DevSystemDEO2::

    ld      a, d
    cp      $08
    jr      nz, .notRed
    call    UpdatePalette
.notRed
    cp      $0a
    jr      nz, .notGreen
    call    UpdatePalette
.notGreen
    cp      $0c
    jr      nz, .notBlue
    call    UpdatePalette
.notBlue

    ret

; d = device
; b = data
DevConsoleDEO::
    ret

; d = device
; bc = data
DevConsoleDEO2::
    ; TODO: write to console
    ret

; d = device
; bc = data
DevTileDEI::
    ret

; d = device
; bc = data
DevTileDEI2::
    ret

; d = device
; b = data
DevTileDEO::
    ld      a, b
    ldh     [hDataByte], a
    ; Determine operation
    ld      a, d
    ldh     [hDeviceByte], a
    sub     $2b
    jr      z, .write
    dec     a
    jr      z, .scrollY
    dec     a
    jr      z, .scrollX

    ret

.scrollY
    ld      a, b
    ldh     [rSCY], a
    ret

.scrollX
    ld      a, b
    ldh     [rSCX], a
    ret

.write
    ld      a, b
    and     %11000000
    jr      z, .tileCopy
    cp      $40
    jr      z, .tilemap
    cp      $80
    ret     nz

.sprite
    ld      hl, wDevices + $24
    ld      a, [hli]    ; x
    ldh     [hX], a
    ld      a, [hli]    ; y
    ldh     [hY], a
    inc     l           ; addr high
    inc     l           ; addr low
    ld      a, [hli]    ; auto (x, y, src, length)
    ld      a, [hli]    ; repeat
    inc     a
    ld      b, a
    ld      c, [hl]     ; tileID

    ; TODO: Handle auto x/y/src/length

    ld      hl, wOAMIndex
    ld      l, [hl]
.repeatSprite
    ldh     a, [hY]
    add     $10
    ld      [hli], a
    ldh     a, [hX]
    add     $08
    ld      [hli], a
    ld      a, c
    ld      [hli], a
    xor     a           ; TODO: flips
    ld      [hli], a

    dec     b
    jr      nz, .repeatSprite

    ; Ensure OAM is updated
    ld      a, HIGH(wShadowOAM)
    ldh     [hOAMHigh], a

    ; TODO: Handle 'reset OAM every frame .sprite is called'

    ret

.tileCopy
    ; Copy tiles from Uxn RAM/ROM to VRAM [blending to be applied later, maybe]
    ld      hl, wDevices + $26
    push    hl
    ld      a, [hli]    ; source addr high
    ld      d, a
    ld      a, [hli]    ; source addr low
    ld      e, a
    ld      a, [hli]    ; auto TODO: do we need autoAddr? Always advance if no blending
    ld      a, [hli]    ; repeat
    inc     a
    ld      b, a
    ld      a, [hl]     ; tileID
    swap    a
    ld      c, a

    ld      hl, $A000   ; offset source addr to GB address
    add     hl, de      ; TODO: Look for "past Uxn space" values
    ld      d, h
    ld      e, l

    ld      h, HIGH($8000)
    ld      l, c

.repeatTiles
    ; Convert CHR to GB 2bpp
    ld      c, 8
.tileLoop
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      a, [de]
    ld      [hli], a
    push    hl
    ld      hl, $0008
    add     hl, de
    ld      a, [hl]
    pop     hl
    ld      [hli], a
    inc     de
    dec     c
    jr      nz, .tileLoop
    dec     b
    jr      nz, .repeatTiles

    ld      hl, -$A000  ; offset back to Uxn address
    add     hl, de
    ld      d, h
    ld      e, l

    pop     hl
    ld      a, d        ; final addr value
    ld      [hli], a
    ld      a, e
    ld      [hl], a
    ret

.tilemap
    ld      a, b
    and     %00000001
    jr      z, .tileIDSource
    ; Copy from addr
    ret

.tileIDSource
    ; Set tilemap to tileID
    ld      hl, wDevices + $24
    ld      a, [hli]    ; x
    ld      c, [hl]    ; y
    push    hl
    ld      h, HIGH($9800)  ; calculate target VRAM addr
    ld      l, a
    ld      a, c
    or      a
    jr      z, .noY
    ld      de, $0020
.yLoop
    add     hl, de
    dec     c
    jr      nz, .yLoop
    ld      d, h
    ld      e, l
.noY
    pop     hl
    inc     l
    inc     l           ; addr high
    inc     l           ; addr low
    ld      a, [hli]    ; auto (x, y, src, length)
    ld      a, [hli]    ; repeat
    inc     a
    ld      b, a
    ld      c, [hl]    ; tileID

    ; TODO: Handle auto x/y/src/length

.repeatTilemap
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      a, c
    ld      [de], a

    dec     b
    jr      nz, .repeatTilemap

    ; store new x/y/id(?)

    ret

; d = device
; bc = data
DevTileDEO2:
    ; Prevent width/height from exceeding 160x144 screen size by resetting width/height after they're set
    ; (I tried handling this in DEI2, but it's handled after the values are pushed to the stack)
    ld      a, d
    sub     $22
    jr      z, .width
    dec     a
    jr      z, .height
    ret
.width
    ld      a, 20
    ld      [wDevices + $22], a
    ret
.height
    ld      a, 18
    ld      [wDevices + $23], a
    ret

DevNil::
    ret

SECTION "Tile Blending", ROM0, ALIGN[8] ; TODO: May not need align[8]

PixelBlendingTable:
    db 0, 0, 1, 2
    db 0, 1, 2, 3
    db 0, 2, 3, 1
    db 0, 3, 1, 1
    db 1, 0, 1, 2
    db 0, 1, 2, 3
    db 1, 2, 3, 1
    db 1, 3, 1, 2
    db 2, 0, 1, 2
    db 2, 1, 2, 3
    db 0, 2, 3, 1
    db 2, 3, 1, 2
    db 3, 0, 1, 2
    db 3, 1, 2, 3
    db 3, 2, 3, 1
    db 0, 3, 1, 2
