#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=17);
input = puzzle.input_data.split("\n")

def creategrid(dims=3):
    grid = set()
    for y in range(len(input)):
        for x in range(len(input[y])):
            if input[y][x] == "#":
                if dims == 3:
                    grid.add((x, y, 0))
                else:
                    grid.add((x, y, 0, 0))
    return grid

def getneighbours3(x, y, z):
    ret = set()
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                if not dx == dy == dz == 0:
                    ret.add((x + dx, y + dy, z + dz))
    return ret

def next3(grid):
    new = set()
    for node in grid:
        neis = getneighbours3(node[0], node[1], node[2])
        aliveneis = 0
        for nei in neis:
            if nei in grid:
                aliveneis += 1
        if 2 <= aliveneis <= 3:
            new.add(node)

        for nei in neis:
            if not nei in grid:
                neineis = getneighbours3(nei[0], nei[1], nei[2])
                aliveneineis = 0
                for neinei in neineis:
                    if neinei in grid:
                        aliveneineis += 1
                if aliveneineis == 3:
                    new.add(nei)
    return new

def getneighbours4(x, y, z, o):
    ret = set()
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            for dz in range(-1, 2):
                for do in range(-1, 2):
                    if not dx == dy == dz == do == 0:
                        ret.add((x + dx, y + dy, z + dz, o + do))
    return ret

def next4(grid):
    new = set()
    for node in grid:
        neis = getneighbours4(node[0], node[1], node[2], node[3])
        aliveneis = 0
        for nei in neis:
            if nei in grid:
                aliveneis += 1
        if 2 <= aliveneis <= 3:
            new.add(node)

        for nei in neis:
            if not nei in grid:
                neineis = getneighbours4(nei[0], nei[1], nei[2], nei[3])
                aliveneineis = 0
                for neinei in neineis:
                    if neinei in grid:
                        aliveneineis += 1
                if aliveneineis == 3:
                    new.add(nei)
    return new

grid3 = creategrid(3)
grid4 = creategrid(4)
for i in range(6):
    grid3 = next3(grid3)
    grid4 = next4(grid4)

silver = len(grid3)
print(f"Silver: {silver}")

gold = len(grid4)
print(f"Gold: {gold}")
