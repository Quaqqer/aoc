#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("01"))
id = puzzle.input_data

# Main code
elves = []

for elf in id.split("\n\n"):
    cals = sum(int(line) for line in elf.split("\n"))
    elves.append(cals)

puzzle.answer_a = max(elves)

puzzle.answer_b = sum(sorted(elves)[-3:])
