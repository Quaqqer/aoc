#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=25)
p_card, p_door = (int(i) for i in puzzle.input_data.split("\n"))

loops_card = 0
while not pow(7, loops_card, 20201227) == p_card: loops_card += 1
silver = pow(p_door, loops_card, 20201227)
print(f"Silver: {silver}")
