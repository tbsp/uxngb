# uxngb

A highly experimental port of the [uxn virtual machine](https://wiki.xxiivv.com/site/uxn.html) to the Game Boy and Game Boy Color gaming handheld. I knew this was a fairly ridiculous project from the start, but wanted to see how it might perform and was pleased to see [compudanza's pong tutorial](https://compudanzas.net/uxn_tutorial_day_6.html) run at slow-motion playable speeds.

No effort has gone into optimizing for performance, aside from trying to avoid writing intentionally bad code. Getting it to run at all was the primary goal. I even optimized for space savings for several of the instructions, because the opportunity for code reuse was too hard to ignore. The MUL and DIV instructions are obviously very slow due to the lack of hardware support for those operations. Unrolling of loops and other approaches could yield significant improvements in speed.

You can download a binary build [here](https://github.com/tbsp/uxngb/releases). Binaries with a variety of UXN ROMs appended are also available there.

## Running your own ROMs

The emulator is contained in the base uxnemu.gbc ROM, and the UXN ROM to be executed must be appended to it. A properly formed GB/GBC ROM will then need to be padded and have the header fixed, which can be performed using `rgbfix` from [RGBDS](https://rgbds.gbdev.io/). This is an example of combining the emulator with a ROM and fixing it from a Linux command prompt:
```
cat uxnemu.gbc dvd.rom > uxnemu_dvd.gbc
rgbfix -O -v -p 0xFF -t dvd uxnemu_dvd.gbc
```

## Implemented

- 253 uxn CPU instructions
- Controller device
- Screen device:
  - Background pixel drawing
  - Background sprite drawing
  - Auto byte for all supported drawing operations
  - Foreground sprites are limited to 16 unique tile/blend combinations, and will begin to overwrite old tiles once this is exceeded
  - Foreground sprites are limited by the 10 sprites/line limit of the hardware (no attempt is made to overcome this via flickering)
- Very basic Datetime device (fixed startup date/time, HH:MM:SS will advance)
- Limited console output (only when built in CLI mode to run CPU instruction test ROM)

## Unsupported
- UXN ROMs larger than ~8 KiB, or UXN memory beyond $2000
  - I originally intended to support the full 64 KiB memory space, using 8 banks of swapped external cartridge RAM, but as the performance I was going to actually end up with revealed itself that dropped in priority
- Screen resizing (fixed at 160x144 pixels)
- Foreground pixel drawing operations aren't currently supported
- Certain blending combinations may not render correctly, and the opaque lookup is not applied
- Stack over/underflow and divide-by-zero are not detected
- No audio/midi/file device support
- No support for relocatable stacks (seen in uxn11)
