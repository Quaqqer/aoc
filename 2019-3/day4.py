from collections import Counter

from aocd.models import Puzzle

from util import ints

puzzle = Puzzle(2019, int("04"))
data = puzzle.input_data

a, b = ints(data.split("-"))


def check_a(v: int) -> bool:
    digits = list(map(int, str(v)))
    counts = Counter(digits)
    return all(v0 <= v1 for v0, v1 in zip(digits, digits[1:])) and any(
        v >= 2 for v in counts.values()
    )


def check_b(v: int) -> bool:
    digits = list(map(int, str(v)))
    counts = Counter(digits)
    return all(v0 <= v1 for v0, v1 in zip(digits, digits[1:])) and any(
        v == 2 for v in counts.values()
    )


puzzle.answer_a = sum([1 for i in range(a, b + 1) if check_a(i)])
puzzle.answer_b = sum([1 for i in range(a, b + 1) if check_b(i)])
