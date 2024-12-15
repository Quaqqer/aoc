# 15   00:15:59   540      0   01:03:12  1176      0
# Tough day today! I was going to make a clean rewrite, but it's a weekend so I
# don't think I will.

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("15"))

data = puzzle.input_data


def solve_a():
    g, instructions = data.split("\n\n")
    g = Grid.from_lines(g)
    [pos] = map(Vec2, g.find_value("@"))
    g[pos] = "."

    for instruction in "".join(instructions.splitlines()):
        dir = {"^": Vec2(0, -1), "<": Vec2(-1, 0), ">": Vec2(1, 0), "v": Vec2(0, 1)}[
            instruction
        ]

        n = pos + dir
        if g[n] == ".":
            pos = n
        elif g[n] == "#":
            continue
        elif g[n] == "O":
            boxes = 1
            can_move = False
            while g[pos + dir * boxes]:
                nn = pos + dir * boxes
                if g[nn] == "O":
                    boxes += 1
                elif g[nn] == "#":
                    can_move = False
                    break
                elif g[nn] == ".":
                    can_move = True
                    break
            if can_move:
                for box in range(boxes)[::-1]:
                    g[pos + dir * (box + 1)] = "O"
                    g[pos + dir * box] = "."
                pos = n

    return sum(100 * y + x for x, y in g.find_value("O"))


def solve_b():
    _g, instructions = data.split("\n\n")

    _g = Grid.from_lines(_g)
    [start] = map(Vec2, _g.find_value("@"))
    _g[start] = "."

    def can_move_boxes(box_pos: Vec2, dir: Vec2) -> bool:
        if g[box_pos] == "]":
            box_pos -= Vec2(1, 0)

        can_move = True
        if dir.y != 0:
            if g[box_pos + dir] in "[]":
                can_move &= can_move_boxes(box_pos + dir, dir)
            if g[box_pos + dir + Vec2(1, 0)] == "[":  # ]
                can_move &= can_move_boxes(box_pos + dir + Vec2(1, 0), dir)
            if g[box_pos + dir] == "#" or g[box_pos + dir + Vec2(1, 0)] == "#":
                can_move = False
        else:
            if dir.x == 1:
                if g[box_pos + dir * 2] == "[":  # ]
                    can_move &= can_move_boxes(box_pos + dir * 2, dir)
                if g[box_pos + dir * 2] == "#":
                    can_move = False
            else:
                if g[box_pos + dir] == "]":
                    can_move &= can_move_boxes(box_pos + dir, dir)
                if g[box_pos + dir] == "#":
                    can_move = False
        return can_move

    def move_boxes(box_pos: Vec2, dir: Vec2):
        if g[box_pos] == "]":
            box_pos -= Vec2(1, 0)

        if dir.y != 0:
            if g[box_pos + dir] in "[]":
                move_boxes(box_pos + dir, dir)
            if g[box_pos + dir + Vec2(1, 0)] == "[":  # ]
                move_boxes(box_pos + dir + Vec2(1, 0), dir)

            g[box_pos + dir] = "["
            g[box_pos + dir + Vec2(1, 0)] = "]"
            g[box_pos] = "."
            g[box_pos + Vec2(1, 0)] = "."
        else:
            if dir.x == 1:
                if g[box_pos + dir * 2] == "[":  # ]
                    move_boxes(box_pos + dir * 2, dir)
                g[box_pos], g[box_pos + Vec2(1, 0)], g[box_pos + Vec2(2, 0)] = (
                    ".",
                    "[",
                    "]",
                )
            elif dir.x == -1:
                if g[box_pos + dir * 2] == "[":  # ]
                    move_boxes(box_pos + dir * 2, dir)
                (
                    g[box_pos + Vec2(-1, 0)],
                    g[box_pos],
                    g[box_pos + Vec2(1, 0)],
                ) = (
                    "[",
                    "]",
                    ".",
                )

    g = Grid.new_fill(lambda: ".", _g.cols * 2, _g.rows)
    for p in map(Vec2, _g):
        if _g[p] == "#":
            g[p * Vec2(2, 1)] = "#"
            g[p * Vec2(2, 1) + Vec2(1, 0)] = "#"
        elif _g[p] == "O":
            g[p * Vec2(2, 1)] = "["
            g[p * Vec2(2, 1) + Vec2(1, 0)] = "]"

    pos = start * Vec2(2, 1)
    for instruction in "".join(instructions.splitlines()):
        dir = {"^": Vec2(0, -1), "<": Vec2(-1, 0), ">": Vec2(1, 0), "v": Vec2(0, 1)}[
            instruction
        ]

        n = pos + dir
        if g[n] == ".":
            pos = n
        elif g[n] == "#":
            continue
        elif g[n] in ["[", "]"]:
            if can_move_boxes(n, dir):
                move_boxes(n, dir)
                pos = n
    return sum(y * 100 + x for x, y in g.find_value("["))


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
