#!/usr/bin/env python3
from aocd.models import Puzzle

import re

# Parse input
puzzle = Puzzle(2021, 2)
lines = puzzle.input_data.split("\n")

# Main code

liner = re.compile("^(forward|up|down) ([0-9]+)$")

horiz = 0
depth_a = 0 # also aim for part b
depth_b = 0

for line in lines:
    match = liner.fullmatch(line)
    assert match
    word = match.group(1)
    n = int(match.group(2))
    
    if word == "up":
        depth_a -= n
    elif word == "down":
        depth_a += n
    elif word == "forward":
        horiz += n
        depth_b += n * depth_a

silver = horiz * depth_a
gold = horiz * depth_b

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}") # type: ignore
    puzzle.answer_a = silver # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}") # type: ignore
    puzzle.answer_b = gold # type: ignore
