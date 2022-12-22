# 22   00:39:11    694      0   03:11:09   1251      0
# no more grids please!

import re
from typing import Callable

from aocd.models import Puzzle

import lib

puzzle = Puzzle(2022, int("22"))
id = puzzle.input_data


def mod(a, b):
    return ((a % b) + b) % b


# made a cube out of paper to make sense of it all,
# I will not be making a general solution
# Eric please stop it!
move_faces: dict[tuple, dict[str, Callable]] = {
    (0, 1): {
        "<": lambda fy, fx: ((2, 0), (49 - fy, 0), ">"),
        "^": lambda fy, fx: ((3, 0), (fx, 0), ">"),
    },
    (0, 2): {
        "^": lambda fy, fx: ((3, 0), (49, fx), "^"),
        ">": lambda fy, fx: ((2, 1), (49 - fy, 49), "<"),
        "v": lambda fy, fx: ((1, 1), (fx, 49), "<"),
    },
    (1, 1): {
        ">": lambda fy, fx: ((0, 2), (49, fy), "^"),
        "<": lambda fy, fx: ((2, 0), (0, fy), "v"),
    },
    (2, 1): {
        ">": lambda fy, fx: ((0, 2), (49 - fy, 49), "<"),
        "v": lambda fy, fx: ((3, 0), (fx, 49), "<"),
    },
    (2, 0): {
        "^": lambda fy, fx: ((1, 1), (fx, 0), ">"),
        "<": lambda fy, fx: ((0, 1), (49 - fy, 0), ">"),
    },
    (3, 0): {
        ">": lambda fy, fx: ((2, 1), (49, fy), "^"),
        "v": lambda fy, fx: ((0, 2), (0, fx), "v"),
        "<": lambda fy, fx: ((0, 1), (0, fy), "v"),
    },
}


map = [line for line in id.splitlines()[:-2]]
maxw = max(len(line) for line in map)
face_size = max(len(map), maxw) // 4
assert face_size == 50
map = [[c for c in line.ljust(face_size * 4)] for line in map]
moves = [
    (int(v[:-1]), v[-1]) for v in re.findall(r"\d+[UDLR]", id.splitlines()[-1])
] + [(45, None)]

rotations = [(0, 1), (1, 0), (0, -1), (-1, 0)]
rotation_draw = [">", "v", "<", "^"]
rotation_draw_inverse = {r: i for i, r in enumerate(rotation_draw)}


def move_step(pos, rotation: int) -> None | tuple[tuple, int]:
    y, x = pos
    dy, dx = rotations[rotation]

    ny, nx = mod(y + dy, face_size * 4), mod(x + dx, face_size * 4)

    if map[ny][nx] == " ":
        f = move_faces[(y // face_size, x // face_size)][rotation_draw[rotation]]
        dpy, dpx = mod(y, face_size), mod(x, face_size)

        (face_y, face_x), (fy, fx), newrot = f(dpy, dpx)

        ny = mod(fy, face_size) + face_size * face_y
        nx = mod(fx, face_size) + face_size * face_x
        assert map[ny][nx] != " "

        rotation = rotation_draw_inverse[newrot]

    if map[ny][nx] == "#":
        return None
    else:
        assert map[ny][nx] in ".><v^"
        return ((ny, nx), rotation)


pos = (0, 0)
while map[pos[0]][pos[1]] == " ":
    pos = lib.tup_add(pos, (0, 1))
rotation = 0

for move, dr in moves:
    for _ in range(move):
        if np := move_step(pos, rotation):
            map[pos[0]][pos[1]] = rotation_draw[rotation]
            pos, rotation = np
        else:
            break

    if dr is not None:
        rotation = mod(rotation + (1 if dr == "R" else -1), 4)
map[pos[0]][pos[1]] = "X"

ans = (pos[0] + 1) * 1000 + (pos[1] + 1) * 4 + rotation

lib.grid.print_grid(map)
print(ans)


assert ans < 141342
puzzle.answer_b = ans
