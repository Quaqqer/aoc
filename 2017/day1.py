#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2017, 1)
input_data = puzzle.input_data


# Main code
sum = 0
for i in range(len(input_data)):
    next_i = 0 if i + 1 == len(input_data) else (i + 1)
    if input_data[i] == input_data[next_i]:
        sum += int(input_data[i])
silver = sum

sum = 0
for i in range(len(input_data)):
    next_i = int((i + len(input_data) / 2) % len(input_data))
    if input_data[i] == input_data[next_i]:
        sum += int(input_data[i])
gold = sum


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
