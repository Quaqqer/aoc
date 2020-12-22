#!/bin/python3

import re
from aocd.models import Puzzle
import queue

puzzle = Puzzle(year=2020, day=20);
data = puzzle.input_data
input = [i.split("\n") for i in data.split("\n\n")]

grids = {}
for i in input:
    id = int(re.match("Tile ([0-9]+):", i[0]).group(1))

    grid = []
    for y in range(1, len(i)):
        xs = []
        for x in range(len(i[y])):
            xs.append(i[y][x])
        grid.append(xs)
    grids[id] = (grid)

def rotated(grid):
    newgrid = [[None for x in range(len(grid[y]))] for y in range(len(grid))]

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            newgrid[x][len(grid)-1-y] = grid[y][x]
    return newgrid

def flippedx(grid):
    newgrid = [list(reversed(line)) for line in grid]
    return newgrid

def flippedy(grid):
    newgrid = list(reversed(grid))
    return newgrid

def permutations(grid):
    grids = []
    for i in range(4):
        grids.append(grid)
        grids.append(flippedx(grid))
        grids.append(flippedy(grid))
        grids.append(flippedx(flippedy(grid)))
        grid = rotated(grid)
    return grids

def getborders(grid):
    b0 = "".join([grid[y][9] for y in range(10)])
    b1 = "".join([grid[9][x] for x in reversed(range(10))])
    b2 = "".join([grid[y][0] for y in reversed(range(10))])
    b3 = "".join([grid[0][x] for x in range(10)])
    return b0, b1, b2, b3

dirs = {0: (1, 0), 1: (0, 1), 2: (-1, 0), 3: (0, -1)}
assembled = {}
assembledids = {}
bordermap = {}

key = next(iter(grids.keys()))
grid = grids.pop(key)
assembled[(0, 0)] = grid
assembledids[(0, 0)] = key

borders = getborders(grid)
for i in range(len(borders)):
    bordermap[borders[i]] = (0, 0, i)

def printgrids():
    minx, miny = 0, 0
    maxx, maxy = 0, 0
    for x, y in assembled.keys():
        if x < minx: minx = x
        if x > maxx: maxx = x
        if y < miny: miny = y
        if y > maxy: maxy = y
    for y in range(miny, maxy + 1):
        print(" " * (2 + (maxx - minx + 1) * 10 + maxx - minx))
        for dy in range(len(grid)):
            print(" ", end="")
            for x in range(minx, maxx + 1):
                line = " " * len(grid)
                if (x, y) in assembled:
                    line = "".join(assembled[(x, y)][dy])
                print(line, end=" ")
            print("")
    print("+" * (2 + (maxx - minx + 1) * 10 + maxx - minx))

q = queue.Queue()
for key in grids:
    if key != assembledids[(0, 0)]:
        q.put(key)

while (not q.empty()):
    key = q.get()
    grid = grids[key]

    match = None
    for perm in permutations(grid):
        borders = getborders(perm)
        bi = -1
        for i in range(len(borders)):
            rev = borders[i][::-1]
            if rev in bordermap:
                if i == (bordermap[rev][2] + 2) % 4:
                    bi = i
                    match = bordermap.pop(rev)

        if match:
            nx, ny, dir = match
            x, y = nx + dirs[dir][0], ny + dirs[dir][1]

            assembled[(x, y)] = perm
            assembledids[(x, y)] = key

            i = 0
            for border in getborders(perm):
                bordermap[border] = (x, y, i)
                i += 1
            break

    if not match:
        q.put(key)

printgrids()
minx, miny = 0, 0
maxx, maxy = 0, 0
for x, y in assembledids.keys():
    if x < minx: minx = x
    if x > maxx: maxx = x
    if y < miny: miny = y
    if y > maxy: maxy = y
silver = assembledids[(minx, miny)] * assembledids[(minx, maxy)] * assembledids[(maxx, miny)] * assembledids[(maxx, maxy)]

def crttotalgrid():
    minx, miny = 0, 0
    maxx, maxy = 0, 0
    for x, y in assembled.keys():
        if x < minx: minx = x
        if x > maxx: maxx = x
        if y < miny: miny = y
        if y > maxy: maxy = y
    
    lines = []
    for y in range(miny, maxy + 1):
        for dy in range(1, len(grid)-1):
            line = []
            for x in range(minx, maxx + 1):
                subline = assembled[(x, y)][dy][1:-1]
                line += subline
            lines.append(line)
    return lines

monster = [[c for c in line] for line in """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """.split("\n")]

def markmonsters(grid):
    found = False
    for x in range(len(grid)-len(monster[0])):
        for y in range(len(grid)-len(monster)):
            ismonster = True
            for mx in range(len(monster[0])):
                for my in range(len(monster)):
                    if monster[my][mx] == "#" and grid[y + my][x + mx] == ".":
                        ismonster = False
                        break
                if not ismonster:
                    break
            if ismonster:
                found = True
                for mx in range(len(monster[0])):
                    for my in range(len(monster)):
                        if monster[my][mx] == "#":
                            grid[y + my][x + mx] = "O"
    return found

gold = 0
tot = crttotalgrid()
totperms = permutations(tot)
for perm in totperms:
    if markmonsters(perm):
        print("\n".join(["".join(line) for line in perm]))
        for y in range(len(perm)):
            for x in range(len(perm)):
                if perm[y][x] == "#":
                    gold += 1
        break

print(f"Silver: {silver}")
print(f"Gold: {gold}")
