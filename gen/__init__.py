#!/usr/bin/env python3

from io import BufferedWriter
from typing import BinaryIO
import struct
import pathlib
import dataclasses
import shutil
import colorsys

import shapedata
import sprites


def shapecolors_hls():
    for idx in range(30):
        hue = (19 * idx * 12 / 360) % 1
        yield (hue, 0.3, 1)
        yield (hue, 0.5, 1)
        yield (hue, 0.7, 1)


def shapecolors():
    def convert(x):
        return min(int(x * 32), 31)

    for hls in shapecolors_hls():
        r, g, b = colorsys.hls_to_rgb(*hls)
        yield Color(convert(r), convert(g), convert(b))

@dataclasses.dataclass
class Color:
    r: int
    g: int
    b: int

    def encode(self) -> bytes:
        mask = 0b11111
        result = 0
        result |= (self.b & mask) << 10
        result |= (self.g & mask) << 5
        result |= (self.r & mask) << 0

        return struct.pack("<H", result)


@dataclasses.dataclass
class Palette:
    colors: list[Color]

    def encode(self) -> bytes:
        return b"".join(c.encode() for c in self.colors)



def playfield() -> bytes:
    tiles = []

    WIDTH = 12
    HEIGHT = 22

    for row in range(32):
        for col in range(32):
            if 16 < col:
                tiles.append(0)
            elif HEIGHT <= row:
                tiles.append(1)
            elif 1 <= col < WIDTH + 1: 
                tiles.append(0)
            else:
                tiles.append(1)

    return b"".join(struct.pack("<H", t) for t in tiles)


def main():
    consts = {}

    out_dir = pathlib.Path("out")
    with (out_dir / "colors.pal").open("wb") as pal_file:
        sc = [*shapecolors()]

        pals = [[Color(0, 0, 0)] + sc[15 * i : 15 * (i + 1)] for i in range(6)]

        p = Palette(sum(pals, []))

        p_encoded = p.encode()
        consts["PALLET_SIZE"] = len(p_encoded)

        pal_file.write(p_encoded)


    with (out_dir / "sprites.vra").open("wb") as f:
        f.write(sprites.all_sprites().encode())

    with (out_dir / "shapes.bin").open("wb") as f:
        consts["SHAPE_COUNT"] = len(shapedata.ALL_ROTATIONS.shapes) // 4
        f.write(shapedata.ALL_ROTATIONS.encode())

    with (out_dir / "playfield.bin").open("wb") as f:
        f.write(playfield())

    with (out_dir / "consts.s").open("w") as f:
        for k, v in consts.items():
            f.write(f"{k} = {v}\n")


if __name__ == "__main__":
    main()
