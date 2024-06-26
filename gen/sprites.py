import dataclasses


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

    @staticmethod
    def binary(asciiart: str) -> "Sprite":
        res = []
        for line in asciiart.splitlines():
            lineout = []
            if not line:
                continue
            for i in range(0, len(line), 2):
                pixel = line[i : i + 2]
                if pixel == "..":
                    lineout.append(0)
                elif pixel == "##":
                    lineout.append(1)
                else:
                    assert False, "Not a valid pixel: '%s'" % pixel
            res.append(lineout)
        return Sprite(res)


@dataclasses.dataclass
class Sprites:
    sprites: list[Sprite]

    def encode(self) -> bytes:
        return b"".join(c.encode() for c in self.sprites)


def tilesprite(idx) -> Sprite:
    l = idx * 3 + 1
    m = idx * 3 + 2
    h = idx * 3 + 3
    return Sprite(
        [
            [h, h, h, h, h, h, h, 0],
            [h, m, m, m, m, m, l, 0],
            [h, m, m, m, m, m, l, 0],
            [h, m, m, m, m, m, l, 0],
            [h, m, m, m, m, m, l, 0],
            [h, m, m, m, m, m, l, 0],
            [h, l, l, l, l, l, l, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
        ]
    )


def blanksprite() -> Sprite:
    return Sprite([[0] * 8] * 8)


def checkersprite() -> Sprite:
    return Sprite(
        [
            [1, 1, 0, 0, 1, 1, 0, 0],
            [1, 1, 0, 0, 1, 1, 0, 0],
            [0, 0, 1, 1, 0, 0, 1, 1],
            [0, 0, 1, 1, 0, 0, 1, 1],
            [1, 1, 0, 0, 1, 1, 0, 0],
            [1, 1, 0, 0, 1, 1, 0, 0],
            [0, 0, 1, 1, 0, 0, 1, 1],
            [0, 0, 1, 1, 0, 0, 1, 1],
        ]
    )


def xsprite() -> Sprite:
    a = 6
    return Sprite(
        [
            [a, a, a, a, a, a, a, 0],
            [a, a, 0, 0, 0, a, a, 0],
            [a, 0, a, 0, a, 0, a, 0],
            [a, 0, 0, a, 0, 0, a, 0],
            [a, 0, a, 0, a, 0, a, 0],
            [a, a, 0, 0, 0, a, a, 0],
            [a, a, a, a, a, a, a, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
        ]
    )


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


def all_sprites() -> Sprites:
    sp = [blanksprite(), xsprite(), *(tilesprite(i) for i in range(5)), *NUMBERS]
    return Sprites(sp)


ZERO = Sprite.binary(
    """
................
......####......
....##....##....
....##....##....
....##....##....
....##....##....
....##....##....
......####......
"""
)


ONE = Sprite.binary(
    """
................
........##......
......####......
........##......
........##......
........##......
........##......
......######....
"""
)

TWO = Sprite.binary(
    """
................
......####......
....##....##....
..........##....
........##......
......##........
....##..........
....########....
"""
)

THREE = Sprite.binary(
    """
................
......####......
....##....##....
..........##....
......####......
..........##....
....##....##....
......####......
"""
)

FOUR = Sprite.binary(
    """
................
..........##....
........####....
......##..##....
....##....##....
....##########..
..........##....
..........##....
"""
)

FIVE = Sprite.binary(
    """
................
....########....
....##..........
....##..........
......####......
..........##....
....##....##....
......####......
"""
)

SIX = Sprite.binary(
    """
................
......####......
....##....##....
....##..........
....######......
....##....##....
....##....##....
......####......
"""
)


SEVEN = Sprite.binary(
    """
................
....########....
..........##....
..........##....
........##......
........##......
......##........
......##........
"""
)


EIGHT = Sprite.binary(
    """
................
......####......
....##....##....
....##....##....
......####......
....##....##....
....##....##....
......####......
"""
)

NINE = Sprite.binary(
    """
................
......####......
....##....##....
....##....##....
......######....
..........##....
..........##....
..........##....
"""
)

NUMBERS = [ZERO, ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE]
