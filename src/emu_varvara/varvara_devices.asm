
SECTION "Device Handlers", ROM0, ALIGN[7]
device_handlers::
    dw dev_system_dei, dev_system_dei2, dev_system_deo, dev_system_deo2 ; system
    dw dev_nil, dev_nil, dev_console_deo, dev_console_deo2              ; console
    dw dev_screen_dei, dev_screen_dei2, dev_screen_deo, dev_screen_deo2 ; screen
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; audio
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; controller
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; mouse
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file0)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file1)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (datetime)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty

SECTION "Varvara Device Defaults", ROM0
device_defaults::
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
    ; screen (0x20)
    dw 0        ; vector
    db HIGH(160), LOW(160)      ; width (big endian)
    db HIGH(144), LOW(144)      ; height (big endian)
    db 0        ; auto
    db 0        ; pad
    dw 0        ; x
    dw 0        ; y
    dw 0        ; addr
    db 0        ; pixel
    db 0        ; sprite
    ; audio
    ds 16*4, 0
    ; midi
    ds 16, 0
    ; controller
    ds 16, 0
    ; mouse
    ds 16, 0
    ; file
    ds 32, 0
    ; datetime
    dw 2022 ; year
    db 1    ; month
    db 1    ; day
    db 3    ; hour
    db 30   ; minute
    db 0    ; second
    db 0    ; dotw
    dw 0    ; doty
    db 0    ; isdst
    ds 5, 0
    ; the rest
    ds 16*3, 0

SECTION "Varvara WRAM", WRAM0, ALIGN[8]
wPixelBlend:    ds 4    ; 4 blend bytes for the current blend mode
wTileBuffer:    ds 16   ; buffer for tile data during blit
wSpriteTileAddr:ds 2    ; address to render sprite tile to

SECTION "Varvara WRAM FG Sprites", WRAM0, ALIGN[8]
wObjSourceAddrs::   ds 16*4 ; blend byte, source UXN address, pad
.end::

SECTION "Varvara HRAM", HRAM
hDeviceByte:        ds 1    ; copy of the device byte for fast access
hDataByte:          ds 1    ; copy of the data byte for fast access
hWorkingBytes:      ds 2    ; stores both the input bytes (initially) and the final pixel data (after blending)
hWorkingX:          ds 1    ; working X coordinate for repeated auto sprite writes
hDeltaY:            ds 1    ; delta Y to apply each auto sprite write
hWorkingY:          ds 1    ; working Y coordinate for repeated auto sprite writes
hDeltaX:            ds 1    ; delta X to apply each auto sprite write
hAutoLen:           ds 1    ; length of auto writes
hAutoAddr:          ds 1    ; auto addr flag
hPixelX:            ds 1    ; current x/y coordinates for pixel drawing (used by pixel and unaligned BG sprite drawinng)
hPixelY:            ds 1
hPixelData:         ds 1
hSpriteUnaligned:   ds 1    ; flag indicating if BG sprite being drawn is unaligned to the grid

SECTION "Varvara Vectors", ROM0

vector_handlers::

    ; controller vector
    ld      hl, devices + $80
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

    ld      hl, uxn_memory
    add     hl, bc

    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a

    call    uxn_eval    ; eval instructions until a BRK is hit
.noControllerVector

    ; screen vector
    ld      hl, devices + $20
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a
    or      c
    jr      z, .noScreenVector

    ld      hl, uxn_memory
    add     hl, bc

    ld      a, h
    ldh     [pc], a
    ld      a, l
    ldh     [pc+1], a

    call    uxn_eval    ; eval instructions until a BRK is hit
.noScreenVector

    ret


SECTION "Varvara Devices", ROM0

dev_system_dei::
    ret

dev_system_dei2::
    ret

; d = device
; b = data
dev_system_deo::

    ; TODO: Check for any writes to RGB range

;     ld      a, d
;     cp      $08
;     jr      nz, .notRed
;     call    updatePalette
; .notRed
;     cp      $0a
;     jr      nz, .notGreen
;     call    updatePalette
; .notGreen
;     cp      $0c
;     jr      nz, .notBlue
;     call    updatePalette
; .notBlue

    ret

; Convert the color values stored in the system device to a host-compatible palette
;  and then queue up a palette update for the next VBlank
updatePalette:

    ldh     a, [hConsoleType]
    or      a
    jr      z, .gbc

    ; For DMG convert the RGB to a 2 bit value
    ; Input: 4bit RGB for each of 4 channels, stored in 6 bytes
    ; Output: 2bit brightness value for each of 4 channels

    ; TODO: Set hOBP0 and hBGP directly

    ret
.gbc

    ; For CGB convert from 12 bit to 15 bit RGB
    ; CGB: xBBBBBGG_GGGRRRRR
    ld      hl, devices + $08
    ld      bc, wPendingPalettes
    call    convertTwoColors
    call    convertTwoColors

    ld      a, 1
    ldh     [hPalettePending], a
.done
    ret

convertTwoColors:
    ; color 0
    ld      a, [hli]    ; red
    inc     l
    and     $f0
    swap    a
    sla     a
    ld      e, a
    ld      a, [hli]    ; green
    inc     l
    and     $f0
    ld      d, a
    sla     a
    or      e
    ld      [bc], a     ; low byte of color 0
    inc     bc
    ld      a, d
    swap    a
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
    sla     a
    ld      e, a
    ld      a, [hli]    ; green
    inc     l
    and     $0f
    ld      d, a
    swap    a
    sla     a
    or      e
    ld      [bc], a     ; low byte of color 1
    inc     bc
    ld      a, d
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
dev_system_deo2::

    ld      a, d
    cp      $08
    jr      nz, .notRed
    call    updatePalette
.notRed
    cp      $0a
    jr      nz, .notGreen
    call    updatePalette
.notGreen
    cp      $0c
    jr      nz, .notBlue
    call    updatePalette
.notBlue

    ret

; d = device
; b = data
dev_console_deo::
    ret

; d = device
; bc = data
dev_console_deo2::
    ; TODO: write to console
    ret

; d = device
; bc = data
dev_screen_dei::
    ret

; d = device
; bc = data
dev_screen_dei2::
    ; TODO: Prevent width/height from exceeding 160x144 screen size
    ret

; d = device
; b = data
dev_screen_deo::
    ld      a, b
    ;inc     a
    ldh     [hDataByte], a
    ; Determine operation
    ld      a, d
    ldh     [hDeviceByte], a
    cp      $2e
    jr      z, .pixel
    cp      $2f
    jp      z, .sprite
    ret

.pixel
    ld      a, b
    bit     6, a
    jp      nz, .pixel_fg

    ; background pixel
    ld      de, devices + $2b   ; low(y)
    ld      a, [de]
    ldh     [hPixelY], a
    dec     e
    dec     e
    ld      a, [de]             ; low(x)
    ldh     [hPixelX], a
    ldh     a, [hDataByte]
    ldh     [hPixelData], a

    call    pixel_draw

    ; auto-advance based on auto flag
    ld      a, [devices + $26]
    bit     0, a
    jr      z, .pixel_noAutoX
    ld      hl, devices + $28
    ld      a, [hli]
    ld      b, a
    ld      a, [hl]
    ld      c, a
    inc     bc
    ld      a, c
    ld      [hld], a
    ld      a, b
    ld      [hl], a
.pixel_noAutoX
    ld      a, [devices + $26]
    bit     1, a
    jr      z, .pixel_noAutoY
    ld      hl, devices + $2a
    ld      a, [hli]
    ld      b, a
    ld      a, [hl]
    ld      c, a
    inc     bc
    ld      a, c
    ld      [hld], a
    ld      a, b
    ld      [hl], a
.pixel_noAutoY

    ret

.pixel_fg
    ret

.sprite

    ; Get BlendingTable values for current blend value combination
    ldh     a, [hDataByte]
    and     $0f         ; only retain blend nibble
    ld      hl, PixelBlendingTable
    sla     a
    sla     a
    add     l
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hli]
    ld      [wPixelBlend], a
    ld      a, [hli]
    ld      [wPixelBlend+1], a
    ld      a, [hli]
    ld      [wPixelBlend+2], a
    ld      a, [hl]
    ld      [wPixelBlend+3], a

    ld      a, [devices + $26]  ; auto
    ld      c, a
    and     $f0
    swap    a
    inc     a       ; inc to make loop simpler
    ldh     [hAutoLen], a
    ld      a, c
    and     %00000100   ; auto addr
    ldh     [hAutoAddr], a
    ld      a, c
    and     %00000001   ; autoX * 8 for deltaX
    sla     a
    sla     a
    sla     a
    ldh     [hDeltaX], a
    ld      a, c
    and     %00000010   ; autoX * 8 for deltaY
    sla     a
    sla     a
    ldh     [hDeltaY], a

    ld      a, b    ; get hDataByte
    bit     6, a
    jp      nz, .sprite_fg

.sprite_bg
    ; background 'sprite'

    ; initialize working x/y coordinates
    ; TODO: Check high byte to see if we should just not render this at all
    ld      hl, devices + $29
    ld      a, [hli]    ; low(x)
    ldh     [hWorkingX], a
    and     %00000111   ; keep unaligned portion
    ld      d, a
    inc     l
    ld      a, [hli]    ; low(y)
    ldh     [hWorkingY], a
    and     %00000111   ; keep unaligned portion
    or      d           ; combine with unaligned Y coordinate
    ldh     [hSpriteUnaligned], a     ; flag to trigger use of (much) slower unaligned sprite drawing approach
    
    ; TODO: Make 1bpp/2bpp both copy the prepared bytes to a buffer which is then
    ;  processed the same, and results in a prepared blob to copy to VRAM either
    ;  aligned or unaligned as needed (handling transparency properly may complicate
    ;  that).
    ldh     a, [hDataByte]
    bit     7, a
    call    z, bgSprite1bpp
    ldh     a, [hDataByte]
    bit     7, a
    call    nz, bgSprite2bpp

    ; perform final auto adjustments

    ; Note: The auto x/y functions in a somewhat unintuitive manner, but is quite clever in allowing
    ;  automatic sprite layout over both dimensions since the opposite deltas are applied during a single
    ;  auto write, and then applied to the named dimensions afterwards, setting up subsequent writes.
    ld      hl, devices + $29 ; low(x)
    ldh     a, [hDeltaX]
    add     [hl]
    ld      [hli], a
    inc     l
    ldh     a, [hDeltaY]
    add     [hl]
    ld      [hl], a

    ret

.sprite_fg
    ; initialize working x/y coordinates
    ; TODO: Check high byte to see if we should just not render this at all
    ld      hl, devices + $29
    ld      a, [hli]    ; low(x)
    ldh     [hWorkingX], a
    inc     l
    ld      a, [hli]    ; low(y)
    ldh     [hWorkingY], a

    ; OAM sprite
    ; - if blend mode 0, simply clear sprite entry
    ; - blit to OAM tile space
    ; - create OAM entry at x/y coords for this tile

    ; Intended approach:
    ; - Check table to see if x/y/addr is present
    ; - If present:
    ;   - if color=0: clear entry
    ;   - if color>0: update OAM entry
    ; - If no present:
    ;   - Render tile to sprite OAM
    ;   - Create OAM entry at X/Y for new tile
    ;   - Add entry to table of sprites

    ldh     a, [hDataByte]
    bit     7, a
    call    z, fgSprite1bpp
    ldh     a, [hDataByte]
    bit     7, a
    call    nz, fgSprite2bpp

    ; perform final auto adjustments

    ld      hl, devices + $29 ; low(x)
    ldh     a, [hDeltaX]
    add     [hl]
    ld      [hli], a
    inc     l
    ldh     a, [hDeltaY]
    add     [hl]
    ld      [hl], a

    ret

; Draw a pixel at hPixelX/hPixelY with color based on hDataByte
; Destroys: AF, BC, HL, DE
pixel_draw:
    ldh     a, [hPixelY]
    cp      144
    ret     nc          ; out of Y range

    ; aligned tile address is:
    ; y/8*20*16
    srl     a   ; y/8
    srl     a
    and     %11111110
    ld      hl, Y_TIMES_320_VRAM
    add     l
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hli]    ; y/8*320
    ld      b, [hl]
    ld      c, a        ; bc=$8000+y/8*320

    ldh     a, [hPixelX]
    cp      160
    ret     nc          ; out of X range
    srl     a           ; x/8
    srl     a
    srl     a
    swap    a           ; x/8*16, overflow bit in lsb
    ld      l, a
    ld      h, 0
    srl     l           ; move possible overflow bit to carry
    rl      h           ; move possible overflow bit to H
    sla     l           ; restore L minus bit
    add     hl, bc

    ; Add non-aligned Y
    ldh     a, [hPixelY]
    and     %00000111   ; retain only sub-tile component
    sla     a           ; double (2 bytes per pixel row)
    ld      c, a
    ld      b, 0
    add     hl, bc

    ldh     a, [hPixelX]
    and     %00000111   ; retain only sub-tile component
    ld      c, a        ; number of bits to shift before pixel insertion

    ld      d, %01111111    ; mask to clear bit
    ld      a, c
    or      a
    jr      z, .mask_ready
    scf
.loop
    rr      d
    dec     c
    jr      nz, .loop
.mask_ready
    ld      a, %11111111
    xor     d
    ld      e, a        ; mask to set bit is the inverse of the mask to clear a bit

    ldh     a, [hPixelData]
    and     $03         ; TODO: What do values above 3 do?
    ld      c, a        ; 0-3 value to write

:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      b, [hl]     ; read current byte value

    srl     c           ; shift lsb into carry to see if we should set or clear bit
    ld      a, b
    jr      c, .set0
    and     d           ; and with clear mask
    jr      .done0
.set0
    or      e           ; and with set mask
.done0
    ld      b, a

:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      [hl], b     ; write new byte value

    ; second byte!
    inc     l
    ld      b, [hl]     ; read current byte value

    srl     c           ; shift lsb into carry to see if we should set or clear bit
    ld      a, b
    jr      c, .set1
    and     d           ; and with clear mask
    jr      .done1
.set1
    or      e           ; and with set mask
.done1
    ld      b, a

:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      [hl], b     ; write new byte value

    ret

locateTargetSpriteTileVRAM:
    ; Current approach:
    ;  - When blend=0, return the tileID of the first entry which matches the source addr
    ;  - The calling code will then hide the first sprite at the given x/y coordinates which
    ;    uses that tileID
    ;  - This will fail if the same tile is blended differently but used at the same x/y
    ;    location, but will at least clear out the invisible sprites

    ; First check for matching entry, while noting the low byte of the last empty entry found
    ldh     a, [hDataByte]
    and     $0f     ; get blend value
    ld      b, a
    ld      hl, devices + $2c   ; get source addr of UXN tile data
    ld      a, [hli]
    ld      d, a
    ld      e, [hl]

    ld      c, $ff  ; low byte of empty entry ($ff=none found)
    ld      hl, wObjSourceAddrs

:   ld      a, [hl]     ; check blend value (0=invisible, and also unused)
    or      a
    jr      nz, .notEmpty
    ld      c, l        ; note low byte of empty entry in case we need it
.notEmpty
    ld      a, b        ; check if blend==0 (erase sprite)
    or      a
    jr      z, .skipBlendCheck
    ld      a, [hl]
    cp      b           ; compare to our target blend value
    jr      nz, .blendDifferent
.skipBlendCheck
    inc     l
    ld      a, [hli]
    cp      d
    jr      nz, .addrDifferent
    ld      a, [hl]
    cp      e
    jr      nz, .addrDifferent
    ; blend and addr match, use the existing VRAM entry!

    ; Return tileID to use for OAM entry in A
    ld      a, l
    srl     a
    srl     a
    add     $f0 ; tileID of 0th foreground tile

    ret

.blendDifferent
.addrDifferent
    ; advance to next entry
    ld      a, l
    or      4-1
    inc     a
    ld      l, a
    cp      LOW(wObjSourceAddrs.end)
    jr      nz, :-

    ; Reached end without finding a match!

    ; Store our blend/addr values in the last empty entry
    ld      l, c    ; low byte of last empty entry
    ld      a, b    ; blend value
    ld      [hli], a
    ld      a, d
    ld      [hli], a
    ld      [hl], e

    ; Calculate VRAM address from table low byte value
    ld      a, c
    sla     a
    sla     a
    ld      c, a
    ld      b, 0
    ld      hl, vForegroundTiles
    add     hl, bc

    ld      a, l
    ld      [wSpriteTileAddr], a
    ld      a, h
    ld      [wSpriteTileAddr+1], a

    ; return A=0 to indicate copy is required
    xor     a

    ret

createOAMEntry:
    ld      e, a    ; cache tileID

    ldh     a, [hDataByte]
    and     $0f     ; get blend value
    jr      nz, .spriteVisible    
    ; sprite blended to zero, locate sprite at this x/y and tileID,
    ;  and hide it
    ldh     a, [hWorkingY]
    ld      b, a
    ldh     a, [hWorkingX]
    ld      c, a

    ; TODO: Flip using hardware flipping, not tile rendering
    ld      hl, wShadowOAM
:   ld      a, [hli]
    cp      b
    jr      nz, .hideSeek
    ld      a, [hli]
    cp      c
    jr      nz, .hideSeek
    ld      a, [hl]
    cp      e
    jr      nz, .hideSeek
    ; Hide this sprite!
    dec     l
    dec     l
    xor     a   ; set y=0 to hide the sprite without affecting the 10spr/line limit
    ld      [hl], a
    jr      .resume
.hideSeek
    ld      a, l
    or      4-1
    inc     a
    ld      l, a
    cp      $A0     ; loop until the end of OAM
    jr      nz, :-
    ; Match not found, UXN software is clearing something it didn't draw,
    ;  or our convoluted hide mechanism got confused. Reset to 0th OAM entry.
    ld      l, 0
    jr      .resume

.spriteVisible
    ld      hl, wOAMIndex       ; point to next unused OAM entry
    ld      l, [hl]

    ldh     a, [hWorkingY]
    add     $10
    ld      [hli], a
    ldh     a, [hWorkingX]
    add     $08
    ld      [hli], a
    ld      a, e
    ld      [hli], a
    xor     a
    ld      [hli], a

.resume
    ld      a, l
    ld      [wOAMIndex], a   ; update next unused OAM entry index

    ; Ensure OAM DMA occurs
    ld      a, HIGH(wShadowOAM)
    ldh     [hOAMHigh], a
    
    ret

fgSprite1bpp:
    call    locateTargetSpriteTileVRAM
    or      a
    jr      nz, .createOAMEntry

    call    tileToBuffer1bpp
    ld      a, [wSpriteTileAddr]
    ld      l, a
    ld      a, [wSpriteTileAddr+1]
    ld      h, a
    call    render1bppTile

.createOAMEntry
    call    createOAMEntry

    ld      d, 8    ; 8 byte offset between 1bpp tiles
    call    applyAutoAdjustments
    jp      nz, fgSprite1bpp

    ret


fgSprite2bpp:
    call    locateTargetSpriteTileVRAM
    or      a
    jr      nz, .createOAMEntry

    call    tileToBuffer2bpp
    ld      a, [wSpriteTileAddr]
    ld      l, a
    ld      a, [wSpriteTileAddr+1]
    ld      h, a
    call    render2bppTile

.createOAMEntry
    call    createOAMEntry

    ld      d, 16
    call    applyAutoAdjustments
    jp      nz, fgSprite2bpp

    ret


; Copy a 1bpp UXN tile pointed to by Screen.addr to the wTileBuffer, including flips
tileToBuffer1bpp:
    ; Locate UXN addr in SRAM (TODO: Account for banks)
    ld      hl, devices + $2c   ; addr
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    ld      hl, uxn_memory
    add     hl, de
    ld      d, h
    ld      e, l
    ; TODO: Handle bank spanning during tile data copy

    ; Copy tile bytes to WRAM buffer to simplify pointer management during blit
    ldh     a, [hDataByte]
    and     %00110000   ; keep only flip bits
    or      a
    jr      z, .noFlip
    cp      %00100000   ; flipy only?
    jr      z, .flipy
    cp      %00010000   ; flipx only?
    jr      z, .flipx
    ; flipx & flipy
    ld      hl, wTileBuffer + $07
    ld      c, 8
:   ld      a, [de]
    ld      b, a
    rlca
    rlca
    xor     b
    and     $AA
    xor     b
    ld      b, a
    rlca
    rlca
    rlca
    rrc     b
    xor     b
    and     $66
    xor     b
    ld      [hld], a
    inc     de
    dec     c
    jr      nz, :-

    jr      .tile_ready
.flipx
    ; flip bit order for bytes as we copy
    ; Based on: http://www.retroprogramming.com/2014/01/fast-z80-bit-reversal.html
    ld      hl, wTileBuffer
    ld      c, 8
:   ld      a, [de]
    ld      b, a
    rlca
    rlca
    xor     b
    and     $AA
    xor     b
    ld      b, a
    rlca
    rlca
    rlca
    rrc     b
    xor     b
    and     $66
    xor     b
    ld      [hli], a
    inc     de
    dec     c
    jr      nz, :-
    jr      .tile_ready
.flipy
    ; Copy bytes backwards
    ld      c, 8
    ld      hl, wTileBuffer + $07
:   ld      a, [de]
    ld      [hld], a
    inc 	de
    dec 	c
    jr 		nz, :-
    jr      .tile_ready
.noFlip
    ld      hl, wTileBuffer
    ld      c, 16
    rst     MemcpySmall
.tile_ready

    ret

; Render a 1bpp tile from wTileBuffer to HL (tile VRAM)
render1bppTile:
    ld      de, wTileBuffer
    ld      b, 8        ; byte counter
.vLoop
    ld      a, [de]     ; setup working bytes for this 8-pixel row
    inc     de
    ldh     [hWorkingBytes], a
    xor     a           ; high byte is always zero for 1bpp
    ldh     [hWorkingBytes+1], a

    push    hl

    ld      c, 8        ; bit counter
.bitLoop
    push    bc
    ldh     a, [hWorkingBytes]
    ld      b, a
    ldh     a, [hWorkingBytes+1]
    ld      c, a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(wPixelBlend)
    ld      l, a
    ld      h, [hl]     ; H = blended pixel value (0-3)

    ldh     a, [hWorkingBytes]
    rr      h           ; shift low bit into carry
    rl      a           ; shift low bit into low working byte
    ldh     [hWorkingBytes], a
    ldh     a, [hWorkingBytes+1]
    rr      h           ; shift high bit into carry
    rl      a           ; shift high bit into high working byte
    ldh     [hWorkingBytes+1], a

    pop     bc
    dec     c
    jr      nz, .bitLoop
    
    pop     hl

    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [hWorkingBytes]
    ld      [hli], a
    ; In some cases (hello-pong) we seem to regularly hit inaccessible VRAM here right after the
    ;  STAT interrupt for the tile bank swap, so be super careful instead.
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [hWorkingBytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .vLoop

    ret


bgSprite1bpp:
    call    tileToBuffer1bpp

    ldh     a, [hSpriteUnaligned]
    or      a
    jr      z, .aligned

    ; unaligned sprites perform 64 subsequent pixel_draw calls! (this is lethally inefficient)
    ldh     a, [hWorkingY]
    ldh     [hPixelY], a
    ldh     a, [hWorkingX]
    ldh     [hPixelX], a

    ld      de, wTileBuffer
    ld      b, 8        ; byte counter
.1bppU_v

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    inc     de
    ldh     [hWorkingBytes], a
    xor     a           ; high byte is always zero for 1bpp
    ldh     [hWorkingBytes+1], a

    push    de
    ld      c, 8        ; bit counter
.1bppU_bit
    push    bc
    ldh     a, [hWorkingBytes]
    ld      b, a
    rla     ; bump bits over a bit for next pass
    ldh     [hWorkingBytes], a
    ldh     a, [hWorkingBytes+1]
    ld      c, a
    rla     ; bump bits over a bit for next pass
    ldh     [hWorkingBytes+1], a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(wPixelBlend)
    ld      l, a
    ld      h, [hl]     ; H = blended pixel value (0-3)

    ld      a, h
    ldh     [hPixelData], a

    call    pixel_draw

    ldh     a, [hPixelX]
    inc     a
    ldh     [hPixelX], a

    pop     bc

.nextPixel
    dec     c
    jr      nz, .1bppU_bit
    pop     de

    ldh     a, [hWorkingX]
    ldh     [hPixelX], a
    ldh     a, [hPixelY]
    inc     a
    ldh     [hPixelY], a

    dec     b
    jr      nz, .1bppU_v
    jr      .autoAdvance

.aligned
    ; Locate target tile VRAM address

    ldh     a, [hWorkingY]
    cp      144
    jp      nc, .yOutOfRange
    ; aligned tile address is:
    ; y/8*20*16
    srl     a   ; y/8
    srl     a
    ;srl     a
    ;sla     a   ; double for table which stores words
    and     %11111110
    ld      hl, Y_TIMES_320_VRAM
    add     l
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hli]    ; y/8*320
    ld      b, [hl]
    ld      c, a        ; bc=$8000+y/8*320

    ldh     a, [hWorkingX]
    cp      160
    jp      nc, .xOutOfRange
    srl     a           ; x/8
    srl     a
    srl     a
    swap    a           ; x/8*16, overflow bit in lsb
    ld      l, a
    ld      h, 0
    srl     l           ; move possible overflow bit to carry
    rl      h           ; move possible overflow bit to H
    sla     l           ; restore L minus bit
    add     hl, bc

    call    render1bppTile

.yOutOfRange
.xOutOfRange
.autoAdvance

    ld      d, 8    ; 8 byte offset between 1bpp tiles
    call    applyAutoAdjustments
    jp      nz, bgSprite1bpp

    ret

; Apply auto x/y/addr:
; Args: D is addr delta to apply in bytes
applyAutoAdjustments:
    ; apply auto adjustments
    ld      hl, hWorkingX
    ld      a, [hli]    ; get hWorkingX
    add     [hl]        ; add hDeltaY (yes)
    dec     l
    ld      [hli], a    ; store new hWorkingX
    inc     l
    ld      a, [hli]    ; get hWorkingY
    add     [hl]        ; add hDeltaX (yes)
    dec     l
    ld      [hli], a    ; store new hWorkingY

    ldh     a, [hAutoAddr]
    or      a
    jr      z, .doneAutoAddr
    ld      hl, devices + $2c
    ld      a, [hli]
    ld      c, [hl]
    ld      b, a
    ld      a, d
    add     c
    ld      c, a
    adc     b
    sub     c
    ld      b, a
    ld      [hl], c
    dec     l
    ld      [hl], b
.doneAutoAddr

    ldh     a, [hAutoLen]
    dec     a
    ldh     [hAutoLen], a

    ret

; Copy a 2bpp UXN tile pointed to by Screen.addr to the wTileBuffer, including flips
tileToBuffer2bpp:

    ; Locate UXN addr in SRAM (TODO: Account for banks)
    ld      hl, devices + $2c   ; addr
    ld      d, [hl]
    inc     l
    ld      e, [hl]
    ld      hl, uxn_memory
    add     hl, de
    ld      d, h
    ld      e, l

    ; Copy tile bytes to WRAM buffer to simplify pointer management during blit
    ldh     a, [hDataByte]
    and     %00110000   ; keep flip bits
    or      a
    jr      z, .noFlip
    cp      %00100000   ; flipy only?
    jr      z, .flipy
    cp      %00010000   ; flipx only?
    jr      z, .flipx
    ; flipx & flipy
    ld      hl, wTileBuffer + $07
    ld      c, 8
:   ld      a, [de]
    ld      b, a
    rlca
    rlca
    xor     b
    and     $AA
    xor     b
    ld      b, a
    rlca
    rlca
    rlca
    rrc     b
    xor     b
    and     $66
    xor     b
    ld      [hld], a
    inc     de
    dec     c
    jr      nz, :-

    ASSERT(HIGH(wTileBuffer) == HIGH(wTileBuffer+$0f))
    ld      l, LOW(wTileBuffer) + $0f
    ld      c, 8

:   ld      a, [de]
    ld      b, a
    rlca
    rlca
    xor     b
    and     $AA
    xor     b
    ld      b, a
    rlca
    rlca
    rlca
    rrc     b
    xor     b
    and     $66
    xor     b
    ld      [hld], a
    inc     de
    dec     c
    jr      nz, :-

    jr      .tile_ready
.flipx
    ; flip bit order for bytes as we copy
    ; Based on: http://www.retroprogramming.com/2014/01/fast-z80-bit-reversal.html
    ld      hl, wTileBuffer
    ld      c, 16
:   ld      a, [de]
    ld      b, a
    rlca
    rlca
    xor     b
    and     $AA
    xor     b
    ld      b, a
    rlca
    rlca
    rlca
    rrc     b
    xor     b
    and     $66
    xor     b
    ld      [hli], a
    inc     de
    dec     c
    jr      nz, :-
    jr      .tile_ready
.flipy
    ; Copy 8 bytes backwards twice (low and high bytes are split, UXN style)
    ld      c, 8
    ld      hl, wTileBuffer + $07
:   ld      a, [de]
    ld      [hld], a
    inc     de
    dec     c
    jr      nz, :-
    ASSERT(HIGH(wTileBuffer) == HIGH(wTileBuffer+$0f))
    ld      l, LOW(wTileBuffer) + $0f
    ld      c, 8
:   ld      a, [de]
    ld      [hld], a
    inc     de
    dec     c
    jr      nz, :-
    jr      .tile_ready
.noFlip
    ld      hl, wTileBuffer
    ld      c, 16
    rst     MemcpySmall
.tile_ready

    ret

; Render a 2bpp tile from DE (wTileBuffer) to HL (tile VRAM)
render2bppTile:
    ld      de, wTileBuffer
    ld      b, 8        ; byte counter
.vLoop

    push    hl

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    ld      hl, $0008   ; UXN tile data isn't interlaced like GB, so we have to span 8 bytes
    add     hl, de
    ldh     [hWorkingBytes], a
    ld      a, [hl]
    ld      de, -$0007  ; setup for next 2bpp byte
    add     hl, de
    ld      d, h
    ld      e, l
    ldh     [hWorkingBytes+1], a

    ; Note: The bit loop is currently the same for 1bpp and 2bpp once the hWorkingBytes are loaded

    ld      c, 8        ; bit counter
.bitLoop
    push    bc
    ldh     a, [hWorkingBytes]
    ld      b, a
    ldh     a, [hWorkingBytes+1]
    ld      c, a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(wPixelBlend)
    ld      l, a
    ld      h, [hl]     ; A = blended pixel value (0-3)

    ldh     a, [hWorkingBytes]
    rr      h           ; shift low bit into carry
    rl      a           ; shift low bit into low working byte
    ldh     [hWorkingBytes], a
    ldh     a, [hWorkingBytes+1]
    rr      h           ; shift high bit into carry
    rl      a           ; shift high bit into high working byte
    ldh     [hWorkingBytes+1], a

    pop     bc
    dec     c
    jr      nz, .bitLoop
    
    pop     hl

    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [hWorkingBytes]
    ld      [hli], a
    ; In some cases (hello-pong) we seem to regularly hit inaccessible VRAM here right after the
    ;  STAT interrupt for the tile bank swap, so be super careful instead.
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [hWorkingBytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .vLoop

    ret

bgSprite2bpp:
    call    tileToBuffer2bpp

    ldh     a, [hSpriteUnaligned]
    or      a
    jr      z, .aligned

    ; unaligned sprites perform 64 subsequent pixel_draw calls! (this is lethally inefficient)
    ldh     a, [hWorkingY]
    ldh     [hPixelY], a
    ldh     a, [hWorkingX]
    ldh     [hPixelX], a

    ld      de, wTileBuffer
    ld      b, 8        ; byte counter
.2bppU_v

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    ld      hl, $0008   ; UXN tile data isn't interlaced like GB, so we have to span 8 bytes
    add     hl, de
    ldh     [hWorkingBytes], a
    ld      a, [hl]
    ld      de, -$0007  ; setup for next 2bpp byte
    add     hl, de
    ld      d, h
    ld      e, l
    ldh     [hWorkingBytes+1], a

    push    de
    ld      c, 8        ; bit counter
.2bppU_bit
    push    bc
    ldh     a, [hWorkingBytes]
    ld      b, a
    rla     ; bump bits over a bit for next pass
    ldh     [hWorkingBytes], a
    ldh     a, [hWorkingBytes+1]
    ld      c, a
    rla     ; bump bits over a bit for next pass
    ldh     [hWorkingBytes+1], a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(wPixelBlend)
    ld      l, a
    ld      h, [hl]     ; H = blended pixel value (0-3)

    ld      a, h
    ldh     [hPixelData], a

    call    pixel_draw

    ldh     a, [hPixelX]
    inc     a
    ldh     [hPixelX], a

    pop     bc

.nextPixel
    dec     c
    jr      nz, .2bppU_bit

    ldh     a, [hWorkingX]
    ldh     [hPixelX], a
    ldh     a, [hPixelY]
    inc     a
    ldh     [hPixelY], a

    pop     de

    dec     b
    jr      nz, .2bppU_v
    jp      .autoAdvance

.aligned
    ; Locate target tile VRAM address

    ldh     a, [hWorkingY]
    cp      144
    jp      nc, .yOutOfRange
    ; aligned tile address is:
    ; y/8*20*16
    srl     a   ; y/8
    srl     a
    ;srl     a
    ;sla     a   ; double for table which stores words
    and     %11111110
    ld      hl, Y_TIMES_320_VRAM
    add     l
    ld      l, a
    adc     h
    sub     l
    ld      h, a
    ld      a, [hli]    ; y/8*320
    ld      b, [hl]
    ld      c, a        ; bc=$8000+y/8*320

    ldh     a, [hWorkingX]
    cp      160
    jp      nc, .xOutOfRange
    srl     a           ; x/8
    srl     a
    srl     a
    swap    a           ; x/8*16, overflow bit in lsb
    ld      l, a
    ld      h, 0
    srl     l           ; move possible overflow bit to carry
    rl      h           ; move possible overflow bit to H
    sla     l           ; restore L minus bit
    add     hl, bc

    call    render2bppTile

.yOutOfRange
.xOutOfRange
.autoAdvance

    ld      d, 16    ; 16 byte offset between 2bpp tiles
    call    applyAutoAdjustments
    jp      nz, bgSprite2bpp

    ret


; d = device
; bc = data
dev_screen_deo2:
    ret

dev_nil::
    ret

SECTION "Y*320 VRAM Table", ROM0, ALIGN[6]
; y*320+$8000 table - used to calculate tile VRAM addresses for 18 possible input values
Y_TIMES_320_VRAM:
dw 0+$8000, 1*320+$8000, 2*320+$8000, 3*320+$8000, 4*320+$8000, 5*320+$8000, 6*320+$8000, 7*320+$8000, 8*320+$8000, 9*320+$8000, 10*320+$8000, 11*320+$8000
; Shift in offset pattern for tiles below the split
dw 13*320+$7FC0, 14*320+$7FC0, 15*320+$7FC0, 16*320+$7FC0, 17*320+$7FC0, 18*320+$7FC0

SECTION "Varvara Blending", ROM0, ALIGN[8] ; TODO: May not need align[8]

; How Varvara blending works:
; - Blending value passed to screen_blit as 'color'
; - Each blending value (0 to f) has an associated opaque value (blending[5] in uxnemu)
; - 'c' (???) calculated as:
;   - 0xAABB (1bpp: AA=0, BB=sprite data, 2bpp: AA=high byte data, BB=low byte data)
; - 'ch' (channel?) calculated as:
;   - 0xCD (C = low bit, D = high bit) -> uxnemu loops through h, taking a bit each time
; - if opaque OR ch, write pixel of color blending[ch][color]

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

; OpaqueTable:
;     db 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0

SECTION "Foreground Tiles", VRAM[$8000]

vBackgroundTilesPrimary:
    ds 16*240

vForegroundTiles:
    ds 16*24

vBackgroundTilesSecondary:
    ds 16*120