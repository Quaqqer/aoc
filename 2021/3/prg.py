#!/usr/bin/env python3
import numpy
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 3)
lines = puzzle.input_data.split("\n")

# Main code

# Part A
nums = len(lines)
bit_counts = sum([numpy.array([int(c) for c in line]) for line in lines])
gamma = "".join([str(int(bc > nums / 2)) for bc in bit_counts])
epsilon = "".join("1" if c == "0" else "0" for c in str(gamma))
silver = int(gamma, 2) * int(epsilon, 2)

# Part B
oxygen = lines.copy()
for i in range(len(oxygen[0])):
    if len(oxygen) == 0:
        break

    common = sum([int(oxygen[x][i]) for x in range(len(oxygen))])
    bit_set = str(int(common >= len(oxygen) / 2))

    oxygen = list(filter(lambda v: v[i] == bit_set, oxygen))
oxygen = oxygen.pop()

scrubber = lines.copy()
for i in range(len(scrubber[0])):
    if len(scrubber) == 0:
        break

    common = sum([int(scrubber[x][i]) for x in range(len(scrubber))])
    if common == len(scrubber) or common == 0:
        continue
    bit_set = str(int(common < len(scrubber) / 2))

    scrubber = list(filter(lambda v: v[i] == bit_set, scrubber))
scrubber = scrubber.pop()

gold = int(oxygen, 2) * int(scrubber, 2)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
