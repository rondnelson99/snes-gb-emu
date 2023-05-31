.INCLUDE "defines.asm"
/* On the Gameboy, we can think of VRAM as divided into eight 1KB blocks:

Blocks 1 and 2 are used for sprite tiles $00-$7F, or optionally background tiles as well
Blocks 3 and 4 are used for sprite AND background tiles $80-$FF00
Blocks 5 and 6 are optionally used for background tiles $00-$7F
Block 7 is the first ($9800) background tilemap
Block 8 is the second ($9C00) background tilemap

The SNES has much more VRAM, but we need to very carefully arrange parts to copy the gaemboy VRAM over directly
- Sprites need to have 4bpp depth, which is tricky


- Sprite tiles start at either $0000, $2000, $4000, $6000
- they should be $800 words long sice they are double size, and $800 bytes on the Gameboy

- Background tilemaps start at a multiple of $800
- background tiles start at a multiple of $1000

for the sprite tiles, we need to make it 4bpp, so we copy the 2bpp data and format the palettes so that every even gameboy tile is visible, and the odd tile data is hidden by the palette
Then, we repeat but copy the odd tiles instead, and shuffle palette IDs when updating OAM.


The VRAM Map:
- $0000-$07FF: 128 4bpp tiles for sprites, or 256 2bpp tiles for bg (When using unsigned addressing)
- $2000-$27FF: 256 2bpp tiles for bg (When using signed addressing)
- $2800: 9800 Tilemap data
- $3000: 9C00 Tilemap data */

.define VRAM_SPRITE_TILES 0x0000 EXPORT
.define VRAM_BG_TILES 0x2000 EXPORT
.define VRAM_BG_TILEMAP_1 0x2800 EXPORT
.define VRAM_BG_TILEMAP_2 0x3000 EXPORT


