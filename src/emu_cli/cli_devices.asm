
SECTION "Device Handlers", ROM0, ALIGN[7]
device_handlers::
    dw dev_system_dei, dev_system_dei2, dev_system_deo, dev_system_deo2 ; system
    dw dev_nil, dev_nil, dev_console_deo, dev_console_deo2              ; console
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file0)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (file1)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty (datetime)
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty
    dw dev_nil, dev_nil, dev_nil, dev_nil                               ; empty

SECTION "UXNCLI Device Defaults", ROM0
device_defaults::
    ds 256, 0

SECTION "UXNCLI Devices", ROM0

dev_system_dei::
    ret

dev_system_dei2::
    ret

; d = device
; b = data
dev_system_deo::
    ; TODO: "Special handling" (set wst/rst?)
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

dev_nil::
    ret