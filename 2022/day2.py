#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("02"))
id = puzzle.input_data

# Main code
score_a = 0

score_b = 0
for line in id.splitlines():
    a, b = tuple(line.split(" "))

    if b == "X":
        b = "A"
    elif b == "Y":
        b = "B"
    elif b == "Z":
        b = "C"

    if b == "A":
        score_a += 1
    elif b == "B":
        score_a += 2
    elif b == "C":
        score_a += 3

    if a == b:
        score_a += 3
    elif a == 'A' and b == 'B':
        score_a += 6
    elif a == 'B' and b == 'C':
        score_a += 6
    elif a == 'C' and b == 'A':
        score_a += 6

for line in id.splitlines():
    a, b = line.split(" ")

    if b == "X":
        b = "A"
    elif b == "Y":
        b = "B"
    elif b == "Z":
        b = "C"

    if b == 'A':
        if a == 'A':
            score_b += 3
        elif a == 'B':
            score_b += 1
        elif a == 'C':
            score_b += 2
        score_b += 0
    elif b == 'B':
        if a == 'A':
            score_b += 1
        elif a == 'B':
            score_b += 2
        elif a == 'C':
            score_b += 3
        score_b += 3
    elif b == 'C':
        if a == 'A':
            score_b += 2
        elif a == 'B':
            score_b += 3
        elif a == 'C':
            score_b += 1
        score_b += 6

puzzle.answer_a = score_a
puzzle.answer_b = score_b
