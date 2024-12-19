# 19   00:05:42   533      0   00:06:40   330      0
# Fun with some easy DP :P

import functools

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("19"))

data = puzzle.input_data

patterns, towels = data.split("\n\n")
patterns = patterns.split(", ")
towels = towels.splitlines()


@functools.cache
def possibles(towel: str) -> int:
    if towel == "":
        return 1

    s = 0
    for p in patterns:
        if towel.startswith(p):
            if x := possibles(towel[len(p) :]):
                s += x
    return s


puzzle.answer_a = sum(possibles(towel) != 0 for towel in towels)
puzzle.answer_b = sum(possibles(towel) for towel in towels)
