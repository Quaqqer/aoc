#!/bin/python3

import math
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=5);
input = puzzle.input_data.split('\n')

def getid(input: str):
    i = input.replace("B", "1").replace("F", "0").replace("R", "1").replace("L", "0")
    return int(i, 2)

ids = set()
for i in input:
    id = getid(i)
    ids.add(id)
print(f"Answer A: {max(ids)}")

def isyours(id):
    return id not in ids and id -1 in ids and id + 1 in ids

ans_b = -1
for id in range(127 * 8 + 7):
    if isyours(id):
        ans_b = id
print(f"Answer B: {ans_b}")
