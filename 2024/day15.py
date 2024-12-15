# 15   00:15:59   540      0   01:03:12  1176      0
# Tough day today! I was going to make a clean rewrite, but it's a weekend so I
# don't think I will.

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("15"))

data = puzzle.input_data

DIR_MAP = {"^": Vec2(0, -1), "<": Vec2(-1, 0), ">": Vec2(1, 0), "v": Vec2(0, 1)}

g, instructions = data.split("\n\n")
instructions = "".join(instructions.splitlines())
g = Grid.from_lines(g)
[start] = map(Vec2, g.find_value("@"))
g[start] = "."

g2 = Grid.new_fill(lambda: ".", g.cols * 2, g.rows)
for p in map(Vec2, g):
    l, r = {"#": "##", "O": "[]", ".": ".."}[g[p]]
    g2[p * Vec2(2, 1)] = l
    g2[p * Vec2(2, 1) + Vec2(1, 0)] = r


def solve_a(g: Grid[str], start: Vec2[int]):
    pos = start

    for instruction in instructions:
        dir = DIR_MAP[instruction]

        n = pos + dir
        if g[n] == ".":
            pos = n
        elif g[n] == "#":
            continue
        elif g[n] == "O":
            boxes = 1
            while g[pos + dir * boxes] == "O":
                boxes += 1
            if g[pos + dir * boxes] == ".":
                for box in range(boxes)[::-1]:
                    g[pos + dir * (box + 1)] = "O"
                    g[pos + dir * box] = "."
                pos = n
                # Don't know why I need to do this...
                g[pos] = "."

    return sum(100 * y + x for x, y in g.find_value("O"))


def solve_b(g: Grid[str], start: Vec2[int]):
    def can_push_box(box_pos: Vec2, dir: Vec2) -> bool:
        if g[box_pos] == "]":
            box_pos -= Vec2(1, 0)

        can_move = True
        if dir.y != 0:
            if g[box_pos + dir] in "[]":
                can_move &= can_push_box(box_pos + dir, dir)
            if g[box_pos + dir + Vec2(1, 0)] == "[":  # ]
                can_move &= can_push_box(box_pos + dir + Vec2(1, 0), dir)
            if g[box_pos + dir] == "#" or g[box_pos + dir + Vec2(1, 0)] == "#":
                can_move = False
        else:
            if dir.x == 1:
                if g[box_pos + dir * 2] == "[":  # ]
                    can_move &= can_push_box(box_pos + dir * 2, dir)
                if g[box_pos + dir * 2] == "#":
                    can_move = False
            else:
                if g[box_pos + dir] == "]":
                    can_move &= can_push_box(box_pos + dir, dir)
                if g[box_pos + dir] == "#":
                    can_move = False
        return can_move

    def push_box(box_pos: Vec2, dir: Vec2):
        if g[box_pos] == "]":
            box_pos -= Vec2(1, 0)

        if dir.y != 0:
            if g[box_pos + dir] in "[]":
                push_box(box_pos + dir, dir)
            if g[box_pos + dir + Vec2(1, 0)] == "[":  # ]
                push_box(box_pos + dir + Vec2(1, 0), dir)

            g[box_pos + dir] = "["
            g[box_pos + dir + Vec2(1, 0)] = "]"
            g[box_pos] = "."
            g[box_pos + Vec2(1, 0)] = "."
        else:
            if dir.x == 1:
                if g[box_pos + dir * 2] == "[":  # ]
                    push_box(box_pos + dir * 2, dir)
                g[box_pos], g[box_pos + Vec2(1, 0)], g[box_pos + Vec2(2, 0)] = (
                    ".",
                    "[",
                    "]",
                )
            elif dir.x == -1:
                if g[box_pos + dir * 2] == "[":  # ]
                    push_box(box_pos + dir * 2, dir)
                (
                    g[box_pos + Vec2(-1, 0)],
                    g[box_pos],
                    g[box_pos + Vec2(1, 0)],
                ) = (
                    "[",
                    "]",
                    ".",
                )

    pos = start
    for instruction in instructions:
        dir = DIR_MAP[instruction]

        n = pos + dir
        if g[n] == ".":
            pos = n
        elif g[n] == "#":
            continue
        elif g[n] in ["[", "]"]:
            if can_push_box(n, dir):
                push_box(n, dir)
                pos = n
    return sum(y * 100 + x for x, y in g.find_value("["))


puzzle.answer_a = solve_a(g.copy(), start)
puzzle.answer_b = solve_b(g2.copy(), start * Vec2(2, 1))
