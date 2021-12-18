#!/usr/bin/env python3
from aocd.models import Puzzle
from typing import Optional, Any
from math import floor, ceil

# Parse input
puzzle = Puzzle(2021, 18)
lines = puzzle.input_data.split("\n")


def to_snail_number(expr: str):
    return eval(expr.replace("[", "(").replace("]", ")"))


snail_numbers = [to_snail_number(e) for e in lines]
expl_test = to_snail_number("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")


# Main code
def add(snumber1, snumber2):
    s = (snumber1, snumber2)

    while True:
        if can_explode(s):
            s, _ = explode(s)
        elif can_split(s):
            s, _ = split(s)
        else:
            break

    return s


def can_explode(snumber, depth=0):
    if isinstance(snumber, int):
        return False

    if depth == 4:
        return True

    assert isinstance(snumber, tuple)
    if can_explode(snumber[0], depth + 1):
        return True
    if can_explode(snumber[1], depth + 1):
        return True
    return False


def add_l(snumber, val):
    if isinstance(snumber, int):
        return snumber + val
    return add_l(snumber[0], val), snumber[1]


def add_r(snumber, val):
    if isinstance(snumber, int):
        return snumber + val
    return snumber[0], add_r(snumber[1], val)


def explode(snumber: Any, depth=0) -> tuple[Any, Optional[tuple[int, int]]]:
    if isinstance(snumber, int):
        return snumber, None

    left, right = snumber
    if depth == 4:
        return 0, (left, right)

    newl, expl = explode(left, depth + 1)
    if expl:
        expl_l, expl_r = expl
        newr = add_l(right, expl_r)
        return (newl, newr), (expl_l, 0)

    newr, expl = explode(right, depth + 1)
    if expl:
        expl_l, expl_r = expl
        newl = add_r(left, expl_l)
        return (newl, newr), (0, expl_r)

    return snumber, None


def can_split(snumber):
    if isinstance(snumber, int):
        return snumber >= 10
    if can_split(snumber[0]):
        return True
    if can_split(snumber[1]):
        return True
    return False


def split(snumber) -> tuple[Any, bool]:
    if isinstance(snumber, int):
        if snumber >= 10:
            return (floor(snumber/2), ceil(snumber/2)), True
        else:
            return snumber, False

    snumber_l, is_split = split(snumber[0])
    if is_split:
        return (snumber_l, snumber[1]), True
    snumber_r, is_split = split(snumber[1])
    if is_split:
        return (snumber[0], snumber_r), True

    return snumber, False


def magnitude(snumber) -> int:
    if isinstance(snumber, int):
        return snumber

    return 3*magnitude(snumber[0]) + 2*magnitude(snumber[1])


cur = snail_numbers[0]
for rhs in snail_numbers[1:]:
    cur = add(cur, rhs)
silver = magnitude(cur)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
