#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("02"))
id = puzzle.input_data


# Main code
def mod(a: int, b: int) -> int:
    return ((a % b) + b) % b


score_a = 0
score_b = 0

translation = {"X": "A", "Y": "B", "Z": "C"}

for line in id.splitlines():
    a, b_ = line.split(" ")
    b = translation[b_]

    # score_a
    # A => 1, B => 2, C => 3
    score_a += ord(b) - 65 + 1

    # Tie if they are the same
    if a == b:
        score_a += 3
    # Win if b is the next character in the sequence [A, B, C] from a
    elif b == chr(mod(ord(a) - 65 + 1, 3) + 65):
        score_a += 6

    # score_b
    if b == "A":
        # Defeat, the next next character (+2)
        score_b += 0 + mod(ord(a) - 65 + 2, 3) + 1
    elif b == "B":
        # Tie, the same charcter (+0)
        score_b += 3 + mod(ord(a) - 65 + 0, 3) + 1
    elif b == "C":
        # Win, the next character (+1)
        score_b += 6 + mod(ord(a) - 65 + 1, 3) + 1

puzzle.answer_a = score_a
puzzle.answer_b = score_b
