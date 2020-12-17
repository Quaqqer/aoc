#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=17);
input = puzzle.input_data.split("\n")

grid = set()
for y in range(len(input)):
    for x in range(len(input[y])):
        if input[y][x] == "#":
            grid.add((x, y, 0))

def getneighbours(x, y, z):
    ret = set()
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                if not dx == dy == dz == 0:
                    ret.add((x + dx, y + dy, z + dz))
    return ret

def next(grid):
    new = set()
    for node in grid:
        neis = getneighbours(node[0], node[1], node[2])
        aliveneis = 0
        for nei in neis:
            if nei in grid:
                aliveneis += 1
        if 2 <= aliveneis <= 3:
            new.add(node)

        for nei in neis:
            if not nei in grid:
                neineis = getneighbours(nei[0], nei[1], nei[2])
                aliveneineis = 0
                for neinei in neineis:
                    if neinei in grid:
                        aliveneineis += 1
                if aliveneineis == 3:
                    new.add(nei)
    return new

def prtgrid(grid, size):
    d2 = [[False for i in range(size)] for i in range(size)]
    for x in range(size):
        for y in range(size):
            if (x, y, 0) in grid:
                d2[y][x] = True
    print("________________________")
    for y in d2:
        print("".join(["#" if i else "." for i in y]))

for i in range(6):
    prtgrid(grid, i + 3)
    grid = next(grid)

print(len(grid))
