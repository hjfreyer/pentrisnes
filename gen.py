#!/usr/bin/env python3

from io import BufferedWriter
from typing import BinaryIO
import struct
import pathlib
import dataclasses
import shutil


@dataclasses.dataclass
class Color:
    r: int
    g: int
    b: int

    def as_short(self) -> int:
        mask = 0b11111
        result = 0
        result |= (self.b & mask) << 10
        result |= (self.g & mask) << 5
        result |= (self.r & mask) << 0

        return result


@dataclasses.dataclass
class Palette:
    colors: list[Color]

    def write_to(self, out: BufferedWriter) -> None:
        for c in self.colors:
            out.write(struct.pack("<H", c.as_short()))


def bitplane_to_rows(bp: list[list[int]]) -> list[int]:
    res = []
    for rowin in bp:
        row = 0
        assert len(rowin) == 8
        for col in rowin:
            row <<= 1
            if col:
                row |= 1
        res.append(row)
    return res


@dataclasses.dataclass
class Sprite:
    data: list[list[int]]

    def bitplane(self, i: int) -> list[list[int]]:
        return [[(col >> i) & 1 for col in row] for row in self.data]

    def encode(self) -> bytes:
        result = [0] * 4 * 8
        rowses = [bitplane_to_rows(self.bitplane(i)) for i in range(4)]

        result[0:16:2] = rowses[0]
        result[1:16:2] = rowses[1]
        result[16:32:2] = rowses[2]
        result[17:32:2] = rowses[3]

        return bytes(result)


@dataclasses.dataclass
class Sprites:
    sprites: list[Sprite]

    def write_to(self, out: BufferedWriter):
        for s in self.sprites:
            out.write(s.encode())


def main():
    out_dir = pathlib.Path("out")
    with (out_dir / "colors.pal").open("wb") as pal_file:
        p = Palette(
            [Color(0, 0, 0), Color(31, 0, 0), Color(0, 0, 31), Color(0, 31, 0)]
            + [Color(0, 0, 0)] * 12
        )

        p.write_to(pal_file)

    sp = Sprite(
        [
            [1, 1, 2, 2, 1, 1, 2, 2],
            [1, 1, 2, 2, 1, 1, 2, 2],
            [2, 2, 1, 1, 3, 3, 1, 1],
            [2, 2, 1, 1, 3, 3, 1, 1],
            [1, 1, 2, 2, 1, 1, 2, 2],
            [1, 1, 2, 2, 1, 1, 2, 2],
            [2, 2, 1, 1, 2, 2, 1, 1],
            [2, 2, 1, 1, 2, 2, 1, 1],
        ]
    )

    with (out_dir / "sprites.vra").open("wb") as f:
        Sprites([sp]).write_to(f)


if __name__ == "__main__":
    main()
