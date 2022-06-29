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


SECTION "Device Handlers", ROM0, ALIGN[7]
DeviceHandlers::
    dw DevSystemDEI, DevSystemDEI2, DevSystemDEO, DevSystemDEO2 ; system
    dw DevNil, DevNil, DevConsoleDEO, DevConsoleDEO2              ; console
    dw dev_tile_dei, dev_tile_dei2, dev_tile_deo, dev_tile_deo2         ; tile
    dw DevNil, DevNil, DevNil, DevNil                               ; audio
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; controller
    dw DevNil, DevNil, DevNil, DevNil                               ; mouse
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (file0)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (file1)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty (datetime)
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty
    dw DevNil, DevNil, DevNil, DevNil                               ; empty

SECTION "Tile Device Defaults", ROM0
DefaultDefaults:::
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
    db 32       ; map tile width (0-255 tiles)
    db 32       ; map tile height (0-255 tiles)
    db 0        ; auto (tile, x, y, 5bit length (32 max))
    db 0        ; x (0-255), used to set bg tile X, object X, and scrollx low byte
    db 0        ; y (0-255), used to set bg tile Y, object Y, and scrolly low byte
    db 0        ; n (0-255), line for split X scroll, number of tiles to copy in tile dump
    db 0        ; tileID (0-255), used to set initial tile for tile copy, BG tile, and object tile
    dw 0        ; tile addr byte to trigger copy
                ;  write start addr of tiles to copy to VRAM
                ;  - tileID sets tile to start copying to
                ;  - x (n?) sets number of tiles to copy (0: 256)
    db 0        ; scroll (first write after vector is X, second is Y) -> Or use extra device byte?
                ; 76543210
                ; |||||||+- scroll0 enable
                ; ||||+++-- scroll0 high bits
                ; |||+----- scroll1 enable (only one scroll direction can be split at a time)
                ; +++------ scroll1 high bits
    db 0        ; sprite (similar to Screen device sprite)
                ; 76543210
                ; ||||++++- blend mode (unsupported on retro platforms)
                ; |||+----- flipx
                ; ||+------ flipy
                ; |+------- bg/object select
                ; +-------- unused (1bpp/2bpp on Varvara)
    db 0
    db 0
    ; Missing: No way to set object palettes or blend modes (which are pretty key to varvara!)
    ; Too much variety in how GB/GBC/NES/SMS/GG handle palettes
    ;  - GB support will never be great once you try to use hardware PPU features
    ; Which features of these platforms might be worth using the extra bytes for?

    ; audio
    ds 16, 0
    ; the rest
    ds 192, 0