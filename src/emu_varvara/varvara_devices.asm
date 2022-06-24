
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
    ds 16, 0
    ; the rest
    ds 192, 0

SECTION "Varvara WRAM", WRAM0, ALIGN[8]
pixel_blend:    ds 4    ; 4 blend bytes for the current blend mode
tile_buffer:    ds 16   ; buffer for tile data during blit

SECTION "Varvara HRAM", HRAM
device_byte:    ds 1    ; copy of the device byte for fast access
data_byte:      ds 1    ; copy of the data byte for fast access
working_bytes:  ds 2    ; stores both the input bytes (initially) and the final pixel data (after blending)
working_x:      ds 1    ; working X coordinate for repeated auto sprite writes
delta_y:        ds 1    ; delta Y to apply each auto sprite write
working_y:      ds 1    ; working Y coordinate for repeated auto sprite writes
delta_x:        ds 1    ; delta X to apply each auto sprite write
auto_len:       ds 1    ; length of auto writes
auto_addr:      ds 1    ; auto addr flag

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

debug_palettes:
    dw 0        ; black
    dw 8935     ; green
    dw 6879     ; orange
    dw 32767    ; white

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
    ld      a, d
    cp      $18
    jr      nz, .notWrite

    ; write character
    ld      a, [cursor_addr]
    ld      h, a
    ld      a, [cursor_addr+1]
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
    ld      [cursor_addr], a
    ld      a, l
    ld      [cursor_addr+1], a
.notWrite
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
    ldh     [data_byte], a
    ; Determine operation
    ld      a, d
    ldh     [device_byte], a
    cp      $2e
    jr      z, .pixel
    cp      $2f
    jp      z, .sprite
    ret

.pixel
    bit     6, a
    jp      nz, .pixel_fg
    ; background pixel

    ld      de, devices + $2b   ; low(y)
    ld      a, [de]
    cp      144
    jr      nc, .pixel_yOutOfRange
    dec     e
    dec     e

    ; aligned tile address is:
    ; y/8*20*16
    ; Note: Taken from sprite aligned code below
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

    ld      a, [de]     ; low(x)
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
    ld      a, [devices + $2b] ; low(y)
    and     %00000111   ; retain only sub-tile component
    sla     a           ; double (2 bytes per pixel row)
    ld      c, a
    ld      b, 0
    add     hl, bc

    ld      a, [devices + $29] ; low(x)
    and     %00000111   ; retain only sub-tile component
    ld      c, a        ; number of bits to shift before pixel insertion

    ld      d, %01111111    ; mask to clear bit
    ld      a, c
    or      a
    jr      z, .mask_ready
    scf
.pixel_loop
    rr      d
    dec     c
    jr      nz, .pixel_loop
.mask_ready
    ld      a, %11111111
    xor     d
    ld      e, a        ; mask to set bit is the inverse of the mask to clear a bit

    ldh     a, [data_byte]
    and     $03         ; TODO: What do values above 3 do?
    ld      c, a        ; 0-3 value to write

:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      b, [hl]     ; read current byte value

    srl     c           ; shift lsb into carry to see if we should set or clear bit
    ld      a, b
    jr      c, .pixel_set0
    and     d           ; and with clear mask
    jr      .pixel_done0
.pixel_set0
    or      e           ; and with set mask
.pixel_done0
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
    jr      c, .pixel_set1
    and     d           ; and with clear mask
    jr      .pixel_done1
.pixel_set1
    or      e           ; and with set mask
.pixel_done1
    ld      b, a

:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ld      [hl], b     ; write new byte value

.pixel_yOutOfRange
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
    bit     6, a
    jp      nz, .sprite_fg
    ; background 'sprite'
    ; - determine if tile-aligned
    ;   - aligned: blit from addr to tile with blending mode
    ;   - unaligned: blit to 2-4 spanning tiles blending mode

.bg_aligned
    ; Get BlendingTable values for current blend value combination
    ldh     a, [data_byte]
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
    ld      [pixel_blend], a
    ld      a, [hli]
    ld      [pixel_blend+1], a
    ld      a, [hli]
    ld      [pixel_blend+2], a
    ld      a, [hl]
    ld      [pixel_blend+3], a

    ld      a, [devices + $26]  ; auto
    ld      c, a
    and     $f0
    swap    a
    inc     a       ; inc to make loop simpler
    ldh     [auto_len], a
    ld      a, c
    and     %00000100   ; auto addr
    ldh     [auto_addr], a
    ld      a, c
    and     %00000001   ; autoX * 8 for deltaX
    sla     a
    sla     a
    sla     a
    ldh     [delta_x], a
    ld      a, c
    and     %00000010   ; autoX * 8 for deltaY
    sla     a
    sla     a
    ldh     [delta_y], a

    ; initialize working x/y coordinates
    ; TODO: Check high byte to see if we should just not render this at all
    ld      hl, devices + $29
    ld      a, [hli]    ; low(x)
    ldh     [working_x], a
    inc     l
    ld      a, [hli]    ; low(y)
    ldh     [working_y], a
    
.bg_auto_loop
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
    ld      hl, tile_buffer
    ld      c, 16
    rst     MemcpySmall

    ldh     a, [working_y]
    cp      144
    jp      nc, .bg_yOutOfRange

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

    ldh     a, [working_x]
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

    ld      de, tile_buffer
    ldh     a, [data_byte]
    bit     7, a    
    jr      nz, .2bpp

    ; 1bpp
    ld      b, 8        ; byte counter
.1bpp_v

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    inc     de
    ldh     [working_bytes], a
    xor     a           ; high byte is always zero for 1bpp
    ldh     [working_bytes+1], a

    push    hl

    ld      c, 8        ; bit counter
.1bpp_bit
    push    bc
    ldh     a, [working_bytes]
    ld      b, a
    ldh     a, [working_bytes+1]
    ld      c, a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(pixel_blend)
    ld      l, a
    ld      h, [hl]     ; A = blended pixel value (0-3)

    ldh     a, [working_bytes]
    rr      h           ; shift low bit into carry
    rl      a           ; shift low bit into low working byte
    ldh     [working_bytes], a
    ldh     a, [working_bytes+1]
    rr      h           ; shift high bit into carry
    rl      a           ; shift high bit into high working byte
    ldh     [working_bytes+1], a

    pop     bc
    dec     c
    jr      nz, .1bpp_bit
    
    pop     hl

    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes]
    ld      [hli], a
    ; In some cases (hello-pong) we seem to regularly hit inaccessible VRAM here right after the
    ;  STAT interrupt for the tile bank swap, so be super careful instead.
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .1bpp_v

    jr      .sprite_auto

.2bpp
    ld      b, 8        ; byte counter
.2bpp_v

    push    hl

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    ld      hl, $0008   ; UXN tile data isn't interlaced like GB, so we have to span 8 bytes
    add     hl, de
    ldh     [working_bytes], a
    ld      a, [hl]
    ld      de, -$0007  ; setup for next 2bpp byte
    add     hl, de
    ld      d, h
    ld      e, l
    ldh     [working_bytes+1], a

    ; Note: The bit loop is currently the same for 1bpp and 2bpp once the working_bytes are loaded

    ld      c, 8        ; bit counter
.2bpp_bit
    push    bc
    ldh     a, [working_bytes]
    ld      b, a
    ldh     a, [working_bytes+1]
    ld      c, a

    xor     a
    sla     c           ; shift high bit into carry
    rl      a           ; shift carry into A
    sla     b           ; shift low bit into carry
    rl      a           ; shift carry into A (now 'ch' 0-3 for a given pixel)

    ld      h, HIGH(pixel_blend)
    ld      l, a
    ld      h, [hl]     ; A = blended pixel value (0-3)

    ldh     a, [working_bytes]
    rr      h           ; shift low bit into carry
    rl      a           ; shift low bit into low working byte
    ldh     [working_bytes], a
    ldh     a, [working_bytes+1]
    rr      h           ; shift high bit into carry
    rl      a           ; shift high bit into high working byte
    ldh     [working_bytes+1], a


    pop     bc
    dec     c
    jr      nz, .2bpp_bit
    
    pop     hl

    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes]
    ld      [hli], a
    ; In some cases (hello-pong) we seem to regularly hit inaccessible VRAM here right after the
    ;  STAT interrupt for the tile bank swap, so be super careful instead.
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .2bpp_v

.bg_yOutOfRange
.sprite_auto
    ; apply auto adjustments
    ld      hl, working_x
    ld      a, [hli]    ; get working_x
    add     [hl]        ; add delta_y (yes)
    dec     l
    ld      [hli], a    ; store new working_x
    inc     l
    ld      a, [hli]    ; get working_y
    add     [hl]        ; add delta_x (yes)
    dec     l
    ld      [hli], a    ; store new working_y

    ldh     a, [auto_addr]
    or      a
    jr      z, .doneAutoAddr
    ld      d, 8    ; addr delta for 1bpp
    ldh     a, [data_byte]
    bit     7, a    
    jr      z, .autoAddr1bpp
    sla     d       ; double addr delta for 2bpp
.autoAddr1bpp
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

    ldh     a, [auto_len]
    dec     a
    ldh     [auto_len], a
    jp      nz, .bg_auto_loop

    ; perform final auto adjustments

    ; Note: The auto x/y functions in a somewhat unintuitive manner, but is quite clever in allowing
    ;  automatic sprite layout over both dimensions since the opposite deltas are applied during a single
    ;  auto write, and then applied to the named dimensions afterwards, setting up subsequent writes.
    ld      hl, devices + $29 ; low(x)
    ldh     a, [delta_x]
    add     [hl]
    ld      [hli], a
    inc     l
    ldh     a, [delta_y]
    add     [hl]
    ld      [hl], a

    ret
    
.sprite_fg
    ; OAM sprite
    ; - blit to OAM tile space
    ; - create OAM entry at x/y coords for this tile
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