# 22   00:39:11    694      0   03:11:09   1251      0
# no more grids please!
import re

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("22"))
id = puzzle.input_data


def mod(a, b):
    return ((a % b) + b) % b


# id = """        ...#
#         .#..
#         #...
#         ....
# ...#.......#
# ........#...
# ..#....#....
# ..........#.
#         ...#....
#         .....#..
#         .#......
#         ......#.
#
# 10R5L5R10L4R5L5"""


def move_step(pos, d):
    y, x = pos
    dy, dx = d

    ny, nx = mod(y + dy, len(map)), mod(x + dx, len(map[y]))
    while map[ny][nx] == " ":
        ny, nx = mod(ny + dy, len(map)), mod(nx + dx, len(map[y]))

    if map[ny][nx] == "#":
        return None
    else:
        return ny, nx


map = [line for line in id.splitlines()[:-2]]
maxw = max(len(line) for line in map)
map = [[c for c in line.ljust(maxw)] for line in map]
moves = [
    (int(v[:-1]), v[-1]) for v in re.findall(r"\d+[UDLR]", id.splitlines()[-1])
] + [(45, None)]

rotations = [(0, 1), (1, 0), (0, -1), (-1, 0)]
rotation_draw = [">", "v", "<", "^"]

pos = (0, 0)
while map[pos[0]][pos[1]] == " ":
    pos = lib.tup_add(pos, (0, 1))
print(pos)
rotation = 0

for move, dr in moves:
    for _ in range(move):
        if np := move_step(pos, rotations[rotation]):
            map[pos[0]][pos[1]] = rotation_draw[rotation]
            pos = np
        else:
            break

    if dr is not None:
        rotation = mod(rotation + (1 if dr == "R" else -1), 4)
map[pos[0]][pos[1]] = "X"

ans = (pos[0] + 1) * 1000 + (pos[1] + 1) * 4 + rotation
print(pos)
print(ans)

lib.grid.print_grid(map)


puzzle.answer_a = ans
# puzzle.answer_b =
