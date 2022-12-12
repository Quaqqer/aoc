# 12   00:38:04   2733      0   00:39:33   2248      0
# was stuck on deques, i thought it would pop from left by default...

from collections import deque

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("12"))
id = puzzle.input_data

# Main code
heights = [[ord(c) - ord("a") for c in row] for row in id.splitlines()]

best_signals = []
for y, row in enumerate(heights):
    for x, height in enumerate(row):
        if height == ord("S") - ord("a"):
            heights[y][x] = 0
            start = (x, y)
        if height == ord("E") - ord("a"):
            heights[y][x] = ord("z") - ord("a")
            end = (x, y)
assert start  # type: ignore
assert end  # type: ignore


def neighbours_(pos):
    x, y = pos
    yield x, y + 1
    yield x, y - 1
    yield x + 1, y
    yield x - 1, y


def neighbours(pos):
    for n in neighbours_(pos):
        x, y = n
        if 0 <= x < len(heights[0]) and 0 <= y < len(heights):
            yield n


def bfs(from_, to):
    queue = deque()
    queue.append((from_, [from_]))
    visited = {from_}

    while queue:
        curr, curr_path = queue.popleft()
        x, y = curr

        if curr == to:
            return curr_path

        for neighbour in neighbours(curr):
            nx, ny = neighbour
            if neighbour not in visited and heights[ny][nx] <= heights[y][x] + 1:
                queue.append((neighbour, curr_path + [neighbour]))
                visited.add(neighbour)

    return None


path = bfs(start, end)
assert path
puzzle.answer_a = len(path) - 1

best_any_a = 10000000000000
for y, row in enumerate(heights):
    for x, height in enumerate(row):
        if height == 0:
            path = bfs((x, y), end)
            if path:
                best_any_a = min(len(path) - 1, best_any_a)

puzzle.answer_b = best_any_a
