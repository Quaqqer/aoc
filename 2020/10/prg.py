#!/bin/python3
from aocd.models import Puzzle
import functools

puzzle = Puzzle(year=2020, day=10);
input = [int(i) for i in puzzle.input_data.split("\n")]
adapters = {i for i in input}

power = 0
delta = 1
threes = 1
ones = 0
while (delta <= 3):
    if power + delta in adapters:
        power = power + delta
        if delta == 3: threes += 1
        if delta == 1: ones += 1
        delta = 1
    else:
        delta += 1
silver = ones * (threes)
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
