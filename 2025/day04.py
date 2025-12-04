# 4   00:07:06   00:08:54
# Always fun with grid puzzles, especially when you have a class for it! 😄
# Although I overslept and woke up 6:01... 😅

# pyright: basic

from aocd.models import Puzzle
from util import Grid

puzzle = Puzzle(2025, int("04"))


def removable(grid: Grid) -> set[tuple[int, int]]:
    return {
        p
        for p in grid.find_value("@")
        if sum(grid[n] == "@" for n in grid.neighbours8(p)) < 4
    }


def solve_a(input: str):
    grid = Grid.from_lines(input)
    return len(removable(grid))


def solve_b(input: str):
    grid = Grid.from_lines(input)
    s = 0
    while r := removable(grid):
        s += len(r)
        for p in r:
            grid[p] = "."
    return s


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
