#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=24);
input = puzzle.input_data.split("\n")

dirs = {
    "e": (1, 0),
    "se": (1, 1),
    "sw": (0, 1),
    "w": (-1, 0),
    "nw": (-1, -1),
    "ne": (0, -1)
}

def todir(st):
    for d in dirs:
        if st.startswith(d):
            return d

tiles = {}
for i in input:
    curr = i
    x, y = 0, 0
    while curr:
        dir = todir(curr)
        curr = curr[len(dir):]
        dx, dy = dirs[dir]
        x += dx
        y += dy
    tiles[(x, y)] = not tiles.get((x, y), False)
silver = sum([1 for tile in tiles.values() if tile])
print(f"Silver: {silver}")

def flip(old):
    tiles = {}
    for tile in old:
        blackns = 0
        for dx, dy in dirs.values():
            tx, ty = tile
            tx += dx
            ty += dy
            blackns += 1 if old.get((tx, ty), False) else 0
        if old.get(tile, False):
            if not (blackns == 0 or blackns > 2):
                tiles[tile] = True
        else:
            if blackns == 2:
                tiles[tile] = True

        oldtile = tile
        for dx, dy in dirs.values():
            tile = oldtile[0] + dx, oldtile[1] + dy
            if tile in tiles:
                continue

            blackns = 0
            for dx, dy in dirs.values():
                tx, ty = tile
                tx += dx
                ty += dy
                blackns += 1 if old.get((tx, ty), False) else 0
            if old.get(tile, False):
                if not (blackns == 0 or blackns > 2):
                    tiles[tile] = True
            else:
                if blackns == 2:
                    tiles[tile] = True
    return tiles

for _ in range(100):
    tiles = flip(tiles)

gold = sum(1 for tile in tiles.values() if tile)
print(f"Gold: {gold}")
