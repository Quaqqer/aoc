#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("06"))
id = puzzle.input_data

# Main code
last_4 = []
for i, char in enumerate(id):
    last_4.append(char)

    if len(last_4) > 4:
        last_4.pop(0)

    if len(set(last_4)) == 4:
        puzzle.answer_a = i + 1
        break

last_14 = []
for i, char in enumerate(id):
    last_14.append(char)

    if len(last_14) > 14:
        last_14.pop(0)

    if len(set(last_14)) == 14:
        puzzle.answer_b = i + 1
        break
