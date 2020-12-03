#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=3);
map = puzzle.input_data.split('\n')

def isTree(x, y):
    line = map[y]
    return line[x % len(line)] == '#'

trees = 0
for y in range(len(map)):
    if isTree(3 * y, y):
        trees += 1
print(f"Answer A: {trees}")

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
res = 1

for dx, dy in slopes:
    trees = 0
    for i in range(0, int(len(map)/dy), 1):
        if isTree(dx * i, i * dy):
            trees += 1

    res *= trees
print(f"Answer B: {res}")
