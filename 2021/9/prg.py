#!/usr/bin/env python3
import queue

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 9)
lines = puzzle.input_data.split("\n")


heights = [[int(c) for c in row] for row in lines]


# Main code
def neighbours(row, col):
    vals = []
    for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        if 0 <= row + dy < len(heights) and 0 <= col + dx < len(heights[0]):
            vals.append((row + dy, col + dx))
    return vals


s = 0
for row in range(len(lines)):
    for col in range(len(lines[0])):
        if all(
            heights[row][col] < heights[nr][nc] for (nr, nc) in neighbours(row, col)
        ):
            s += heights[row][col] + 1

silver = s


def basin(row, col) -> int:
    size = 0
    visited = set()
    q = queue.Queue()
    q.put((row, col))

    while not q.empty():
        r, c = q.get()
        size += 1
        for n in neighbours(r, c):
            if n not in visited and heights[n[0]][n[1]] < 9:
                visited.add(n)
                q.put(n)
    return size - 1


lows = []
for row in range(len(lines)):
    for col in range(len(lines[0])):
        if all(
            heights[row][col] < heights[nr][nc] for (nr, nc) in neighbours(row, col)
        ):
            lows.append((row, col))
basins = [basin(r, c) for (r, c) in lows]
basins.sort()
print(basins)
gold = basins[-1] * basins[-2] * basins[-3]


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
