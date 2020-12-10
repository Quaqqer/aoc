#!/bin/python3
from aocd.models import Puzzle
import functools

puzzle = Puzzle(year=2020, day=10);
input = sorted([int(i) for i in puzzle.input_data.split("\n")])
adapters = {i for i in input}

power = 0
additions = {1: 0, 2:0, 3:0}
for i in input:
    if i <= power + 3:
        additions[i - power] += 1
        power = i
silver = additions[1] * (additions[3] + 1)
print(f"Silver: {silver}")

@functools.cache
def arrangements(goal, curr=0):
    sum = 0
    for i in range(1, 4):
        if curr + i == goal:
            sum += 1
        elif curr + i in adapters:
            sum += arrangements(goal, curr+i)
    return sum

gold = arrangements(power)
print(f"Gold: {gold}")
