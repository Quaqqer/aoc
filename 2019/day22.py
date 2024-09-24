from __future__ import annotations

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2019, int("22"))
data = puzzle.input_data
lines = data.splitlines()


def find_new_index(card: int, n_cards: int, lines: list[str]) -> int:
    for line in lines:
        if line.startswith("cut"):
            [n] = ints(line)
            card = (card + n_cards - n) % n_cards
        elif line.startswith("deal with increment"):
            [n] = ints(line)
            card = (card * n) % n_cards
        elif line.startswith("deal into new stack"):
            card = (n_cards - card - 1) % n_cards
        else:
            raise Exception("Unknown case")
    return card


class Mat:
    """A matrix superior to numpy because it doesn't overflow"""

    def __init__(self, values: list[list[int]]):
        self.values = values

        self.rows = len(self.values)
        self.cols = len(self.values[0])

    def matmul(self, other: Mat, mod: int) -> Mat:
        cols = other.cols
        rows = self.rows

        result = Mat([[0 for _ in range(cols)] for _ in range(rows)])

        for x in range(cols):
            for y in range(rows):
                n = len(self.values[y])
                assert len(other.values) == n

                for i in range(n):
                    result[y, x] += self[y, i] * other[i, x]
                    result[y, x] %= mod

        return result

    def pow(self, n: int, mod: int) -> Mat:
        assert n >= 0

        if n == 0:
            return Mat([[1, 0], [0, 1]])

        res = self.pow(n // 2, mod)

        if n % 2 == 0:
            return res.matmul(res, mod)
        else:
            return res.matmul(res, mod).matmul(self, mod)

    def __getitem__(self, coord: tuple[int, int]) -> int:
        y, x = coord
        return self.values[y][x]

    def __setitem__(self, coord: tuple[int, int], v: int):
        y, x = coord
        self.values[y][x] = v


def solve_a() -> int:
    m = Mat([[1, 0], [0, 1]])
    n_cards = 10_007

    for line in data.splitlines():
        match line.split():
            case "cut", n:
                m = m.matmul(Mat([[1, 0], [-int(n), 1]]), n_cards)
            case "deal", "with", "increment", n:
                m = m.matmul(Mat([[int(n), 0], [0, 1]]), n_cards)
            case "deal", "into", "new", "stack":
                m = m.matmul(Mat([[-1, 0], [-1, 1]]), n_cards)

    return Mat([[2019, 1]]).matmul(m, n_cards)[0, 0]


def solve_b() -> int:
    m = Mat([[1, 0], [0, 1]])
    n_cards = 119_315_717_514_047

    for line in data.splitlines()[::-1]:
        match line.split():
            case "cut", n:
                m = m.matmul(Mat([[1, 0], [int(n), 1]]), n_cards)
            case "deal", "with", "increment", n:
                mod_inv = pow(int(n), -1, n_cards)
                m = m.matmul(Mat([[mod_inv, 0], [0, 1]]), n_cards)
            case "deal", "into", "new", "stack":
                m = m.matmul(Mat([[-1, 0], [-1, 1]]), n_cards)

    m = m.pow(101_741_582_076_661, n_cards)

    return Mat([[2020, 1]]).matmul(m, n_cards)[0, 0]


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
