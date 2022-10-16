#!/usr/bin/env python3
import re
from collections import defaultdict
from typing import Optional, TypeVar

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2019, 4)
lines = puzzle.input_data.split("\n")


# Main code
T = TypeVar("T")


def not_none(v: Optional[T]) -> T:
    assert v is not None
    return v


def criteria(pas: list[int], gold: bool) -> bool:
    if not all(x1 <= x2 for x1, x2 in zip(pas, pas[1:])):
        return False

    counts = defaultdict(int)

    for v in pas:
        counts[v] += 1

    if (gold and 2 not in counts.values()) or (
        not gold and not any(x >= 2 for x in counts.values())
    ):
        return False

    return True


lower, higher = [int(m) for m in not_none(re.match(r"(\d+)-(\d+)", lines[0])).groups()]
nums = [[int(c) for c in str(num)] for num in range(lower, higher + 1)]
silver_passes = [pas for pas in nums if criteria(pas, False)]
gold_passes = [pas for pas in nums if criteria(pas, True)]

silver = len(silver_passes)
gold = len(gold_passes)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
