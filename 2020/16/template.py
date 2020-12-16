#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=16);
input = [i.split("\n") for i in puzzle.input_data.split("\n\n")]

attrex = re.compile("(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)")
valids = {i: False for i in range(1000)}
fields = {}

for i in input[0]:
    mtch = attrex.match(i)
    for j in range(int(mtch.group(2)), int(mtch.group(3)) + 1):
        valids[j] = True
    for j in range(int(mtch.group(4)), int(mtch.group(5)) + 1):
        valids[j] = True
    fields[mtch.group(1)] = (int(mtch.group(2)), int(mtch.group(3)), int(mtch.group(4)), int(mtch.group(5)))

correct = []

errors = 0
for i in input[2][1:]:
    lst = i.split(",")
    error = False
    for j in lst:
        if not valids[int(j)]:
            errors += int(j)
            error = True
    if not error:
        correct.append(i)

silver = errors
print(f"Silver {silver}")

def possib(j):
    poss = set()
    for field, ranges in fields.items():
        if ranges[0] <= j <= ranges[1] or ranges[2] <= j <= ranges[3]:
            poss.add(field)
    return poss

possibles = {}
for i in correct:
    lst = [int(i) for i in i.split(",")]
    for j in range(len(lst)):
        if j in possibles:
            possibles[j] = possibles[j].intersection(possib(lst[j]))
        else:
            possibles[j] = possib(lst[j])

change = True
while (change):
    change = False
    for i in possibles:
        for j in possibles:
            if len(possibles[i]) == 1 and len(possibles[j]) != 1:
                possibles[j] = possibles[j] - possibles[i]
                if len(possibles[j]) == 1:
                    change = True
fieldsx = [i.pop() for i in possibles.values()]
prod = 1
myvals = [int(i) for i in input[1][1].split(",")]
for i in range(len(fieldsx)):
    if fieldsx[i].startswith("departure"):
        prod *= myvals[i]
gold = prod
print(f"Gold {gold}")
