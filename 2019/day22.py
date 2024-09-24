# I LOVE MATH!!!!!!!!!!!!!! It's amazing how you can simplify this assignment
# if you know the proper math, which I don't, but with some help of from my
# friends we came up with this

from __future__ import annotations

from aocd.models import Puzzle

puzzle = Puzzle(2019, int("22"))
data = puzzle.input_data
lines = data.splitlines()


class Mat:
    """A matrix superior to numpy because it doesn't overflow"""

    def __init__(self, values: list[list[int]]):
        self.values = values

        self.rows = len(self.values)
        self.cols = len(self.values[0])

    def __repr__(self) -> str:
        col_widths = [
            max(len(str(self[y, x])) for y in range(self.rows))
            for x in range(self.cols)
        ]

        return (
            "["
            + "\n ".join(
                "["
                + ", ".join(
                    str(self[y, x]).rjust(col_widths[x]) for x in range(self.cols)
                )
                + "]"
                for y in range(self.rows)
            )
            + "]"
        )

    def __matmul__(self, other: Mat) -> Mat:
        cols = other.cols
        rows = self.rows

        result = Mat([[0 for _ in range(cols)] for _ in range(rows)])

        for x in range(cols):
            for y in range(rows):
                n = len(self.values[y])
                assert len(other.values) == n

                for i in range(n):
                    result[y, x] += self[y, i] * other[i, x]

        return result

    def __mod__(self, mod: int) -> Mat:
        return Mat([[(v % mod) for v in row] for row in self.values])

    def pow_mod(self, n: int, mod: int) -> Mat:
        assert n >= 0

        if n == 0:
            return Mat([[1, 0], [0, 1]])

        res = self.pow_mod(n // 2, mod)

        if n % 2 == 0:
            return (res @ res) % mod
        else:
            return (res @ res @ self) % mod

    def __mul__(self, scalar: int) -> Mat:
        return Mat([[v * scalar for v in row] for row in self.values])

    def det(self) -> int:
        assert self.cols == 2 and self.rows == 2
        return self[0, 0] * self[1, 1] - self[0, 1] * self[1, 0]

    def modular_inverse(self, mod: int) -> Mat:
        assert self.cols == 2 and self.rows == 2
        return Mat([[self[1, 1], -self[0, 1]], [-self[1, 0], self[0, 0]]]) * pow(
            self.det(), -1, mod
        )

    def __getitem__(self, coord: tuple[int, int]) -> int:
        y, x = coord
        return self.values[y][x]

    def __setitem__(self, coord: tuple[int, int], v: int):
        y, x = coord
        self.values[y][x] = v


A_CARDS = 10_007
B_CARDS = 119_315_717_514_047
B_REPETITIONS = 101_741_582_076_661

m = Mat([[1, 0], [0, 1]])

for line in data.splitlines():
    match line.split():
        case "cut", n:
            m @= Mat([[1, 0], [-int(n), 1]])
        case "deal", "with", "increment", n:
            m @= Mat([[int(n), 0], [0, 1]])
        case "deal", "into", "new", "stack":
            m @= Mat([[-1, 0], [-1, 1]])


puzzle.answer_a = (Mat([[2019, 1]]) @ m)[0, 0] % A_CARDS

# Since part two is to find the new card at index 2020 instead of where 2019
# ended up we take the modular inverse of the matrix, it's like magic.
m2 = m.modular_inverse(B_CARDS)
# And raise it to the power of the amount of repetitions
m2 = m2.pow_mod(B_REPETITIONS, B_CARDS)
puzzle.answer_b = (Mat([[2020, 1]]) @ m2)[0, 0] % B_CARDS
