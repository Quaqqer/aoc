#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 25)
lines = puzzle.input_data.split("\n")


initial = tuple(tuple(col for col in line) for line in lines)


# Main code
def next(state):
    height = len(state)
    width = len(state[0])

    old_state = state
    new_state = [[state[row][col] for col in range(width)] for row in range(height)]
    for y in range(len(old_state)):
        for x in range(len(old_state[y])):
            next_x = (x + 1) % width
            if old_state[y][x] == ">" and old_state[y][next_x] == ".":
                new_state[y][x] = "."
                new_state[y][next_x] = ">"
    old_state = tuple(map(tuple, new_state))
    for y in range(len(old_state)):
        for x in range(len(old_state[y])):
            next_y = (y + 1) % height
            if old_state[y][x] == "v" and old_state[next_y][x] == ".":
                new_state[y][x] = "."
                new_state[next_y][x] = "v"
    return tuple(map(tuple, new_state))


def print_state(state):
    for y in range(len(state)):
        print("".join(state[y]))


visited = {initial}
i = 0
state = initial
while True:
    state = next(state)
    i += 1
    if state in visited:
        silver = i
        break
    visited.add(state)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
