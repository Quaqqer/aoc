# Modified code to run faster
# Runs 10x faster than my original implementation because it doesn't use my
# grid class which is apparently really slow...
#
# It did however inspire me to improve my grid class. Apparently it's a lot
# faster to use [row[:] for row in rows] to copy the grid than using deepcopy,
# not really surprising since deepcopy also copies values, but still...

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("06"))
g = [list(line) for line in puzzle.input_data.splitlines()]
W = len(g[0])
H = len(g)


def walk(g: list[list[str]], start: tuple[int, int]) -> tuple[bool, list[list[str]]]:
    x, y = start
    dx, dy = 0, -1

    visited: set[tuple[int, int, int, int]] = set()

    while True:
        g[y][x] = "X"

        # Check if we have already visited this position
        if ((x, y, dx, dy)) in visited:
            return True, g
        visited.add((x, y, dx, dy))

        nx, ny = x + dx, y + dy

        # Check if we go out of bounds
        if not (0 <= nx < W and 0 <= ny < H):
            return False, g

        # Check if we should rotate
        elif g[ny][nx] == "#":
            dx, dy = -dy, dx

        # Move otherwise
        else:
            x, y = nx, ny


def is_infinite_with_barrier(
    g: list[list[str]], start: tuple[int, int], barrier: tuple[int, int]
):
    bx, by = barrier
    g[by][bx] = "#"
    infinite, _ = walk(g, start)
    return infinite


[s] = ((x, y) for y in range(H) for x in range(W) if g[y][x] == "^")


_, ag = walk([l[:] for l in g], s)
puzzle.answer_a = sum(ag[y][x] == "X" for y in range(H) for x in range(W))
puzzle.answer_b = sum(
    is_infinite_with_barrier([l[:] for l in g], s, (x, y))
    for y in range(H)
    for x in range(W)
    if ag[y][x] == "X"
)
