# 15   00:15:59   540      0   01:03:12  1176      0
# Tough day today! Grid problems with larger structures than a single tile are
# quite annoying tbh.

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

        match g[pos + dir]:
            case '.':
                pos += dir
            case 'O':
                boxes = 1
                while g[pos + dir * (boxes + 1)] == 'O':
                    boxes += 1
                if g[pos + dir * (boxes + 1)] == '.':
                    for box in range(1, boxes + 1)[::-1]:
                        g[pos + dir * box] = '.'
                        g[pos + dir * (box + 1)]= 'O'
                    pos += dir

    return sum(100 * y + x for x, y in g.find_value("O"))


def push_box_b(g: Grid[str], box_pos: Vec2, dir: Vec2) -> bool:
    if g[box_pos] == "]":
        box_pos -= Vec2(1, 0)

    l, r = box_pos, box_pos + Vec2(1, 0)

    ok = True

    if dir.y != 0:
        if g[l + dir] == "#" or g[r + dir] == "#":
            return False

        # Check for box left sides to the left below, below and right below
        # Us -> [
        #      [[[
        for delta in [Vec2(-1, 0), Vec2(0, 0), Vec2(1, 0)]:
            if g[l + delta + dir] == "[":
                ok &= push_box_b(g, l + delta + dir, dir)

        g[l], g[r] = ".", "."
        g[l + dir], g[r + dir] = "[", "]"
    else:
        if g[l + dir if dir.x == -1 else l + 2 * dir] == "#":
            return False
        if g[l + dir * 2] == "[":
            ok &= push_box_b(g, l + dir * 2, dir)
        g[l], g[r] = ".", "."
        g[l + dir], g[r + dir] = "[", "]"

    return ok


def solve_b(g: Grid[str], start: Vec2[int]):
    pos = start
    for instruction in instructions:
        dir = DIR_MAP[instruction]

        n = pos + dir
        if g[n] == ".":
            pos = n
        elif g[n] == "#":
            continue
        elif g[n] in "[]":
            gc = g.copy()
            if push_box_b(gc, n, dir):
                g = gc
                pos = n
    return sum(y * 100 + x for x, y in g.find_value("["))


puzzle.answer_a = solve_a(g.copy(), start)
puzzle.answer_b = solve_b(g2.copy(), start * Vec2(2, 1))
