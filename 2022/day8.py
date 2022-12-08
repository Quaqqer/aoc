#!/usr/bin/env python3
from aocd.models import Puzzle

puzzle = Puzzle(2022, int("08"))
id = puzzle.input_data

heights = {}

width = 0
height = 0
for y, row in enumerate(id.splitlines()):
    height = max(height, y + 1)
    for x, cell in enumerate(row):
        width = max(x + 1, width)
        heights[(x, y)] = int(cell)


def visible_from_edge(from_, dir):
    x, y = from_
    dx, dy = dir

    last = heights[(x, y)]
    vis = {from_}

    x, y = x + dx, y + dy

    while 0 <= x < width and 0 <= y < height:

        curr = heights[(x, y)]

        if last < curr:
            vis.add((x, y))
            last = curr

        x, y = x + dx, y + dy

    return vis


visible_from_edges = set()
for x in range(width):
    visible_from_edges.update(
        visible_from_edge((x, 0), (0, 1)) | visible_from_edge((x, height - 1), (0, -1))
    )
for y in range(height):
    visible_from_edges.update(
        visible_from_edge((0, y), (1, 0)) | visible_from_edge((width - 1, y), (-1, 0))
    )

puzzle.answer_a = len(visible_from_edges)


def visible_from_tree(from_, dir) -> set[tuple[int, int]]:
    x, y = from_
    dx, dy = dir
    h = heights[from_]
    vis = set()

    x, y = x + dx, y + dy
    while 0 <= x < width and 0 <= y < height:

        curr = heights[(x, y)]

        if h > curr:
            vis.add((x, y))
        else:
            vis.add((x, y))
            break
        x, y = x + dx, y + dy

    return vis


def scenic_view(pos):
    return (
        len(visible_from_tree(pos, (0, -1)))
        * len(visible_from_tree(pos, (0, 1)))
        * len(visible_from_tree(pos, (-1, 0)))
        * len(visible_from_tree(pos, (1, 0)))
    )


puzzle.answer_b = max(
    max(scenic_view((x, y)) for x in range(width)) for y in range(height)
)
