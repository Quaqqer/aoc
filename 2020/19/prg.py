#!/bin/python3

import re
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=19);
data = puzzle.input_data
input = [i.split("\n") for i in data.split("\n\n")]

rules = {}
for i in input[0]:
    mtch = re.match("([0-9]+): \"([a-z])\"", i)
    if mtch:
        rules[int(mtch.group(1))] = mtch.group(2)
    else:
        mtch = re.match("([0-9]+): (.*)", i)
        rules[int(mtch.group(1))] = [[int(y) for y in re.findall("([0-9]+)", x)] for x in mtch.group(2).split("|")]


def verify(lines, rule=0):
    poss = []
    if isinstance(rules[rule], str):
        for line in lines:
            if line and rules[rule] == line[0]:
                poss.append(line[1:])
    else:
        for line in lines:
            for r in rules[rule]:
                curr = [line]
                for sr in r:
                    if not curr:
                        break
                    curr = verify(curr, sr)
                poss += curr
    return poss


silver = sum([1 for i in input[1] if "" in verify([i])])
print(f"Silver: {silver}")

rules[8] = [[42], [42, 8]]
rules[11] = [[42, 31], [42, 11, 31]]

gold = sum([1 for i in input[1] if "" in verify([i])])
print(f"Gold: {gold}")
