
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
;blend_bytes:    ds 4    ; 4 blend bytes for the current blend mode
;source_bytes:   ds 2    ; source bytes for pixel blend
working_bytes:  ds 2    ; stores both the input bytes (initially) and the final pixel data (after blending)

SECTION "Varvara Devices", ROM0

dev_system_dei::
    ret

dev_system_dei2::
    ret

; d = device
; b = data
dev_system_deo::
    ; TODO: "Special handling" (set wst/rst/palette)
    ret

; d = device
; bc = data
dev_system_deo2::
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
    ret

; d = device
; b = data
dev_screen_deo::
    ld      a, b
    ldh     [data_byte], a
    ; Determine operation
    ld      a, d
    ldh     [device_byte], a
    cp      $2f
    jr      z, .sprite
    ret
.sprite
    bit     6, a
    jp      nz, .fg
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

    ; TODO: Check high byte to see if we should just not render this at all
    ld      de, devices + $2b   ; low(y)
    ld      a, [de]
    dec     e
    dec     e

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

    ld      c, 8        ; bit counter
.1bpp_bit
    push    bc
    ldh     a, [working_bytes]
    ld      b, a
    ldh     a, [working_bytes+1]
    ld      c, a

    push    hl

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

    pop     hl

    pop     bc
    dec     c
    jr      nz, .1bpp_bit
    
    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes]
    ld      [hli], a
    ldh     a, [working_bytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .1bpp_v

    ret

.2bpp
    ld      b, 8        ; byte counter
.2bpp_v

    ld      a, [de]     ; setup working bytes for this 8-pixel row
    inc     de
    ldh     [working_bytes], a
    ld      a, [de]
    inc     de
    ldh     [working_bytes+1], a

    ; Note: The bit loop is currently the same for 1bpp and 2bpp once the working_bytes are loaded

    ld      c, 8        ; bit counter
.2bpp_bit
    push    bc
    ldh     a, [working_bytes]
    ld      b, a
    ldh     a, [working_bytes+1]
    ld      c, a

    push    hl

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

    pop     hl

    pop     bc
    dec     c
    jr      nz, .2bpp_bit
    
    ; working byte is now ready, copy to VRAM
:   ldh     a, [rSTAT]
    and     STATF_BUSY
    jr      nz, :-
    ldh     a, [working_bytes]
    ld      [hli], a
    ldh     a, [working_bytes+1]
    ld      [hli], a

    dec     b
    jr      nz, .2bpp_v

    ret
    
.fg
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