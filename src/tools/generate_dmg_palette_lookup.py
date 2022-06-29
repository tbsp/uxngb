#!/usr/bin/env python

#
# DMG Palette Lookup Generator
#
# Copyright 2022 Dave VanEe
#
# This software is provided 'as-is', without any express or implied
# warranty.  In no event will the authors be held liable for any damages
# arising from the use of this software.
# 
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
# 
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
#

import math

output = 'res/dmg_palette_lookup_generated.asm'

def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

with open(output, 'w') as f:
    VALUES = 64

    f.write('SECTION "DMG Palette Lookup", ROM0\n')
    f.write('DMGPaletteLookup:\n')

    lookupValues = []
    for value in range(VALUES):
        # Break out into RGB components
        red = (value & 0b00110000) << 2
        green = (value & 0b00001100) << 4
        blue = (value & 0b00000011) << 6

        intensity = (0.2989*red + 0.5870*green + 0.1140*blue)

        # Scale intensity back up because we're only using values from 0-191,
        #  and if we don't the 2bit intensity won't be full range.
        #intensity = intensity * 256./192.

        # Further scale and then cap to avoid light colors not showing up
        #intensity = min(intensity * 0.8, 255)

        intensity = min(intensity * 1.2, 255)

        # Invert because of how DMG palette values work
        intensity = 255 - intensity

        lookupValues.append(int(intensity) >> 6)

    print(len(lookupValues))
    for chunk in chunks(lookupValues, 16):
        f.write('    db {}\n'.format(','.join(['${:02X}'.format(0xFF & int(item)) for item in chunk])))
        print('    db {}'.format(','.join(['${:02X}'.format(0xFF & int(item)) for item in chunk])))
