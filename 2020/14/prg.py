#!/bin/python3

import re
import itertools
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=14);
input = puzzle.input_data.split("\n")

valuex = re.compile("([0-9X]+)$")

mask = ""
memory = {}
for i in input:
    value = valuex.search(i).group(1)
    if i.startswith("mask"):
        mask = value
    elif i.startswith("mem"):
        pos = int(re.search("\\[([0-9]+)\\]", i).group(1))
        value = [c for c in bin(int(value))[2:].zfill(36)]
        for j in range(36):
            if mask[j] != "X":
                value[j] = mask[j]
        memory[pos] = int("".join(value), 2)

silver = sum(memory.values())
print(f"Silver: {silver}")

#silver =

mask = ""
memory = {}
for i in input:
    value = valuex.search(i).group(1)
    if i.startswith("mask"):
        mask = value
    elif i.startswith("mem"):
        pos = [c for c in bin(int(re.search("\\[([0-9]+)\\]", i).group(1)))[2:].zfill(36)]
        value = int(value)
        xis = []
        for j in range(len(pos)):
            c = mask[j]
            if c == "X":
                xis.append(j)
            elif c == "1":
                pos[j] = "1"
        for perm in itertools.product([0, 1], repeat=len(xis)):
            for j in range(len(perm)):
                pos[xis[j]] = str(perm[j])
            memory[int("".join(pos), 2)] = value

gold = sum(memory.values())
print(f"Gold: {gold}")
puzzle.answer_b = gold
