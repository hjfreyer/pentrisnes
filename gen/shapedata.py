import dataclasses
import math


@dataclasses.dataclass
class ColoredShape:
    shape: "Shape"
    color: int

    def encode(self) -> bytes:
        ints = [self.color]
        ints += [row * 32 + col for row, col in self.shape.offsets]
        ints += [ints[1]] * (8 - len(ints))
        return b"".join(o.to_bytes(2, "little", signed=True) for o in ints)

    def rotate(self, count) -> "ColoredShape":
        return ColoredShape(color=self.color, shape=self.shape.rotate(count))


@dataclasses.dataclass
class Shape:
    offsets: list[tuple[int, int]]

    def mirrorY(self) -> "Shape":
        return Shape(offsets=[(row, -col) for row, col in self.offsets])

    def _boundingbox(self) -> tuple[int, int, int, int]:
        rows = [row for row, col in self.offsets]
        cols = [col for row, col in self.offsets]
        return (
            min(rows),
            min(cols),
            max(rows),
            max(cols),
        )

    def center(self) -> tuple[float, float]:
        minRow, minCol, maxRow, maxCol = self._boundingbox()

        # The side length of a square surrounding the shape. Assumes shapes are
        # always oriented horizontally.
        squareSide = maxCol - minCol
        rowOffsetIntoSquare = math.ceil((squareSide - (maxRow - minRow)) / 2)
        squareMinRow = minRow - rowOffsetIntoSquare
        centerRow = squareMinRow + squareSide / 2
        centerCol = minCol + squareSide / 2

        return (centerRow, centerCol)

    def rotate(self, count) -> "Shape":
        centerRow, centerCol = self.center()

        centered = [(row - centerRow, col - centerCol) for row, col in self.offsets]
        rotated = centered
        for _ in range(count):
            rotated = [(col, -row) for row, col in rotated]
        return Shape(
            [(int(row + centerRow), int(col + centerCol)) for row, col in rotated]
        )


@dataclasses.dataclass
class Shapes:
    shapes: list[ColoredShape]

    def encode(self) -> bytes:
        return b"".join(c.encode() for c in self.shapes)


# Pentominos.

pentominoF = Shape(
    [
        (0, 0),
        (1, 0),
        (0, 1),
        (0, 2),
        (-1, 1),
    ]
)

pentominoI = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (0, 3),
        (0, 4),
    ]
)

pentominoL = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (0, 3),
        (1, 3),
    ]
)

pentominoN = Shape(
    [
        (1, 0),
        (1, 1),
        (0, 1),
        (0, 2),
        (0, 3),
    ]
)

pentominoP = Shape(
    [
        (0, 0),
        (1, 0),
        (0, 1),
        (1, 1),
        (1, -1),
    ]
)

pentominoT = Shape(
    [
        (0, -1),
        (0, 1),
        (0, 0),
        (1, 0),
        (2, 0),
    ]
)

pentominoU = Shape(
    [
        (0, 0),
        (0, -1),
        (0, 1),
        (-1, 1),
        (-1, -1),
    ]
)

pentominoV = Shape(
    [
        (0, -2),
        (0, -1),
        (0, 0),
        (1, 0),
        (2, 0),
    ]
)

pentominoW = Shape(
    [
        (0, 0),
        (0, 1),
        (1, 1),
        (1, 2),
        (2, 2),
    ]
)

pentominoX = Shape(
    [
        (0, 0),
        (-1, 0),
        (1, 0),
        (0, -1),
        (0, 1),
    ]
)

pentominoY = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (0, 3),
        (-1, 1),
    ]
)

pentominoZ = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (-1, 0),
        (1, 2),
    ]
)

pentominoF2: Shape = pentominoF.mirrorY()
pentominoJ: Shape = pentominoL.mirrorY()
pentominoN2: Shape = pentominoN.mirrorY()
pentominoQ: Shape = pentominoP.mirrorY()
pentominoY2: Shape = pentominoY.mirrorY()
pentominoS: Shape = pentominoZ.mirrorY()

PENTOMINOES: list[Shape] = [
    pentominoF,
    pentominoF2,
    pentominoL,
    pentominoJ,
    pentominoN,
    pentominoN2,
    pentominoP,
    pentominoQ,
    pentominoY,
    pentominoY2,
    pentominoZ,
    pentominoS,
    pentominoT,
    pentominoU,
    pentominoV,
    pentominoW,
    pentominoX,
    pentominoI,
]

# Tetrominos

tetrominoI = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (0, 3),
    ]
)

tetrominoJ = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (1, 2),
    ]
)

tetrominoL = tetrominoJ.mirrorY()

tetrominoO = Shape(
    [
        (0, 0),
        (0, 1),
        (1, 0),
        (1, 1),
    ]
)

tetrominoS = Shape(
    [
        (1, 0),
        (1, 1),
        (0, 1),
        (0, 2),
    ]
)

tetrominoZ = tetrominoS.mirrorY()

tetrominoT = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
        (1, 1),
    ]
)

TETROMINOES = [
    tetrominoI,
    tetrominoJ,
    tetrominoL,
    tetrominoO,
    tetrominoS,
    tetrominoZ,
    tetrominoT,
]

# Trominoes

trominoI = Shape(
    [
        (0, 0),
        (0, 1),
        (0, 2),
    ]
)

trominoL = Shape(
    [
        (0, 0),
        (0, 1),
        (1, 1),
    ]
)

TROMINOES = [trominoI, trominoL]

domino = Shape(
    [
        (0, 0),
        (0, 1),
    ]
)

monomino = Shape([(0, 0)])

ALL_SHAPES = [
    monomino,
    domino,
    *TROMINOES,
    *TETROMINOES,
    *PENTOMINOES,
]


def allShapes() -> list[ColoredShape]:
    res = []
    for i, shape in enumerate(ALL_SHAPES):
        i %= 30
        tile = i % 5 + 2
        palette = i // 5
        color = (palette << 10) | tile

        centerRow, centerCol = shape.center()
        centered = Shape(
            offsets=[
                (row - int(centerRow), col - int(centerCol))
                for row, col in shape.offsets
            ]
        )

        res.append(ColoredShape(color=color, shape=centered))
    return res


ALL_ROTATIONS = Shapes([s.rotate(i) for s in allShapes() for i in range(4)])
