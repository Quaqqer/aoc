#!/usr/bin/env python3
import re
from collections import defaultdict
from copy import deepcopy
from typing import Any

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("05"))
id = puzzle.input_data

# Main code
crates, moves = id.split("\n\n")

stacks = defaultdict(list)

# Reverse lines to add to stacks bottom up
for line in crates.splitlines()[:-1][::-1]:
    # Get all stack items
    row = [line[i + 1] for i in range(0, len(line), 4)]
    for i, col in enumerate(row):
        # If the column is not empty
        if col != " ":
            # Add the item to both stacks
            stacks[i + 1].append(col)

stacks_a = deepcopy(stacks)
stacks_b = deepcopy(stacks)


def move_a(n: int, from_: int, to: int) -> None:
    # N times
    for _ in range(n):
        # Remove an item from the from stack and add it to the to stack
        stacks_a[to].append(stacks_a[from_].pop())


def move_b(n: int, from_: int, to: int) -> None:
    # Add the last n elements from the from stack to to the to stack
    stacks_b[to] = stacks_b[to] + stacks_b[from_][-n:]
    # Remove n items from the from stack
    stacks_b[from_] = stacks_b[from_][:-n]


for move_line in moves.splitlines():
    m = re.match(r"move (\d+) from (\d+) to (\d+)", move_line)
    assert m
    n, from_, to = map(int, m.groups())

    # Do moves
    move_a(n, from_, to)
    move_b(n, from_, to)


def ans(stacks: dict[Any, list[str]]) -> str:
    """Format a set of stacks into a answer string"""
    # Relies on insertion order, works fine here
    return "".join([stack[-1] for stack in stacks.values()])


print(ans(stacks_a), ans(stacks_b))
puzzle.answer_a = ans(stacks_a)
puzzle.answer_b = ans(stacks_b)
