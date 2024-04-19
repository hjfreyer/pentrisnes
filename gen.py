#!/usr/bin/env python3

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

    def write_to(self, out: BinaryIO):
        for c in self.colors:
            out.write(struct.pack("<H", c.as_short()))


def main():
    out_dir = pathlib.Path("out")
    with (out_dir / "colors.pal").open("wb") as pal_file:
        p = Palette(
            [Color(0, 0, 0), Color(31, 0, 0), Color(0, 0, 31)] + [Color(0, 0, 0)] * 13
        )

        p.write_to(pal_file)


if __name__ == "__main__":
    main()
