#!/bin/python3

import math
from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=5);
input = puzzle.input_data.split('\n')

def getid(input):
    miny = 0
    maxy = 127
    for i in range(7):
        delta = maxy - miny
        if input[i] == 'F':
            maxy -= math.ceil(delta/2)
        else:
            miny += math.ceil(delta/2)
    minx = 0
    maxx = 7
    for i in range(7, 10):
        delta = maxx - minx
        if input[i] == 'L':
            maxx -= math.ceil(delta/2)
        else:
            minx += math.ceil(delta/2)

    return maxy * 8 + minx


ids = set()
for i in input:
    id = getid(i)
    ids.add(id)
print(f"Answer A: {max(ids)}")

def isyours(id):
    for i in range(0, id):
        if i in ids:
            break
    else:
        return False

    for i in range(id + 1, 127 * 8 + 7):
        if i in ids:
            break
    else:
        return False

    return id not in ids and id -1 in ids and id + 1 in ids

ans_b = -1
for id in range(127 * 8 + 7):
    if isyours(id):
        ans_b = id
print(f"Answer B: {ans_b}")
