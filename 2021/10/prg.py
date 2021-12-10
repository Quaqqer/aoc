#!/usr/bin/env python3
from typing import Optional

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 10)
lines = puzzle.input_data.split("\n")

# Main code
corrupt_scores = {
    ")": 3,
    "]": 57,
    "}": 1197,
    ">": 25137,
}

unclosed_scores = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}


def corrupt(line) -> Optional[str]:
    """Return first corrupt char, otherwise none."""
    stack = list()

    for char in line:
        if char == "(":
            stack.append(")")
        elif char == "[":
            stack.append("]")
        elif char == "{":
            stack.append("}")
        elif char == "<":
            stack.append(">")
        elif len(stack) > 0 and char == stack[-1]:
            stack.pop()
        else:
            return char
    return None


def unclosed(line) -> Optional[list[str]]:
    """Return stack if unclosed, otherwise none."""
    stack: list[str] = list()

    for char in line:
        if char == "(":
            stack.append(")")
        elif char == "[":
            stack.append("]")
        elif char == "{":
            stack.append("}")
        elif char == "<":
            stack.append(">")
        elif len(stack) > 0 and char == stack[-1]:
            stack.pop()
        else:
            return None  # ignore corrupt lines
    return list(reversed(stack))


def score_unclosed(unclosed):
    score = 0
    for u in unclosed:
        score *= 5
        score += unclosed_scores[u]
    return score


silver = sum(corrupt_scores[c] for c in map(corrupt, lines) if c)


us = sorted([score_unclosed(u) for u in map(unclosed, lines) if u])
gold = us[len(us) // 2]


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
