#!/usr/bin/env python3
import queue
from functools import reduce

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 9)
lines = puzzle.input_data.split("\n")


# Main code
heights = [[int(c) for c in row] for row in lines]
rows = len(heights)
cols = len(heights[0])

neighbouring_deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]


def in_bounds(row, col):
    return 0 <= row < rows and 0 <= col < cols


def neighbours(row, col):
    return [
        (row + dr, col + dc)
        for (dr, dc) in neighbouring_deltas
        if in_bounds(row + dr, col + dc)
    ]


def all_cells():
    for row in range(rows):
        for col in range(cols):
            yield (row, col)


lows = [
    (cr, cc)
    for (cr, cc) in all_cells()
    if all(heights[cr][cc] < heights[nr][nc] for (nr, nc) in neighbours(cr, cc))
]
silver = sum(heights[cr][cc] + 1 for (cr, cc) in lows)


def basin_size(row, col) -> int:
    visited = {(row, col)}
    q = queue.Queue()
    q.put((row, col))

    size = 0

    while not q.empty():
        size += 1

        r, c = q.get()
        for (nr, nc) in neighbours(r, c):
            if (nr, nc) not in visited and heights[nr][nc] < 9:
                visited.add((nr, nc))
                q.put((nr, nc))
    return size


best_basins = sorted([basin_size(r, c) for (r, c) in lows])[-3:]
gold = reduce(int.__mul__, best_basins)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
