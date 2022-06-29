# uxngb

A highly experimental port of the [uxn virtual machine](https://wiki.xxiivv.com/site/uxn.html) to the Game Boy and Game Boy Color gaming handheld. I knew this was a fairly ridiculous project from the start, but wanted to see how it might perform and was pleased to see [compudanza's pong tutorial](https://compudanzas.net/uxn_tutorial_day_6.html) run at slow-motion playable speeds.

No effort has gone into optimizing for performance, aside from trying to avoid writing intentionally bad code. Getting it to run at all was the primary goal. I even optimized for space savings for several of the instructions, because the opportunity for code reuse was too hard to ignore. The MUL and DIV instructions are obviously very slow due to the lack of hardware support for those operations. Unrolling of loops and other approaches could yield significant improvements in speed.

You can download a binary build [here](https://github.com/tbsp/uxngb/releases). Binaries with a variety of UXN ROMs appended are also available there.

- `hello-*` UXN ROMs built using code from [compudanza's uxn tutorial series](https://compudanzas.net/uxn_tutorial.html), used under the [peer production license](https://wiki.p2pfoundation.net/Peer_Production_License).
- All other UXN ROMs built (some with minor modifications to fit the GB screen) using code from [100 Rabbits](https://git.sr.ht/~rabbits/uxn/tree/main), copyright (c) Devine Lu Linvega under the [MIT license](https://opensource.org/licenses/MIT).

## Screenshots

![uxnemu_catclock](https://user-images.githubusercontent.com/10489588/176349457-68669912-c901-4946-8060-08c3a0110e2c.png)
![uxnemu_cube3d](https://user-images.githubusercontent.com/10489588/176349461-5277d505-db2a-4ffa-af0c-b0430a1ed340.png)
![uxnemu_hello-pong-1](https://user-images.githubusercontent.com/10489588/176349464-3727bd2c-67e9-4df0-92a6-a40ab6a4e89b.png)
![uxnemu_mandelbrot](https://user-images.githubusercontent.com/10489588/176349469-3e76b4fc-2706-45e8-88cc-ae6aaf0a76fa.png)
![uxnemu_mandelbrot-1](https://user-images.githubusercontent.com/10489588/176349474-a9f18eaa-e7d2-4f73-9778-4a9923cf7ae8.png)
![uxnemu_screen](https://user-images.githubusercontent.com/10489588/176349485-183ba2f7-24dc-4623-a9db-0d9f5aadd741.png)

## Performance

The mandlebrot ROM currently takes ~1h44m to render fully on an original Game Boy, and ~52min on a Game Boy Color (using double-speed mode).

## Running your own ROMs

The emulator is contained in the base `uxnemu.gbc` ROM, and the UXN ROM to be executed must be appended to it. To create a properly formed GB/GBC ROM, it will then need to be padded and have the header fixed, which can be performed using `rgbfix` from [RGBDS](https://rgbds.gbdev.io/). This is an example of combining the emulator with a ROM and fixing it from a Linux command prompt:
```
cat uxnemu.gbc dvd.rom > uxnemu_dvd.gbc
rgbfix -O -v -p 0xFF -t dvd uxnemu_dvd.gbc
```

## Implemented

- All 253 uxn CPU instructions
- Controller device
- Screen device:
  - Background pixel drawing
  - Background sprite drawing
  - Auto byte for all supported drawing operations
  - Foreground sprites are limited to 16 unique tile/blend combinations, and will begin to overwrite old tiles once this is exceeded (sprites are flipped in hardware, so flip variations don't count toward this limit)
  - Foreground sprites are limited by the 10 sprites/line limit of the hardware (no attempt is made to overcome this via flickering)
- Very basic Datetime device (fixed startup date/time, HH:MM:SS will advance)
- Limited console output (only when built in CLI mode to run CPU instruction test ROM)

## Unsupported
- UXN ROMs larger than ~8 KiB, or UXN memory beyond $2000
  - I originally intended to support the full 64 KiB memory space, using 8 banks of swapped external cartridge RAM, but as the performance I was going to actually end up with revealed itself that dropped in priority
- Screen resizing (fixed at 160x144 pixels)
- Setting the System RGB bytes has no effect on Game Boy
- Foreground pixel drawing operations aren't currently supported
- Certain blending combinations may not render correctly, and the opaque lookup is not applied
- Stack over/underflow and divide-by-zero are not detected
- No keyboard/mouse/audio/midi/file device support
- No support for relocatable stacks (seen in uxn11)
