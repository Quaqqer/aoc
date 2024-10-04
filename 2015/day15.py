from math import prod
from typing import Iterable

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2015, int("15"))
data = puzzle.input_data
lines = data.splitlines()

cookies = [ints(line) for line in lines]
assert len(cookies) == 4


def cookie_combination() -> Iterable[tuple[int, int]]:
    for i in range(101):
        for j in range(101 - i):
            for k in range(101 - i - j):
                l = 100 - i - j - k

                ms = []
                for ing in range(4):
                    ms.append(
                        max(
                            sum(
                                ing * m
                                for ing, m in zip(
                                    tuple(zip(*cookies))[ing], [i, j, k, l]
                                )
                            ),
                            0,
                        )
                    )

                calories = sum(
                    ing * m for ing, m in zip(tuple(zip(*cookies))[4], [i, j, k, l])
                )

                yield prod(ms), calories


puzzle.answer_a = max(score for score, _ in cookie_combination())
puzzle.answer_b = max(
    score for score, calories in cookie_combination() if calories == 500
)
