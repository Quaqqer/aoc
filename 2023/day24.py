# 24   00:33:02   563      0   02:12:28   940      0
# I have succumbed to Z3. It kind of feels like cheating, but I tried solving
# it and I would sit here all day ¯\_(ツ)_/¯, and in Sweden we celebrate
# christmas eve and not christmas day. If there is another way I would love to
# solve it myself. Merry christmas!

from collections import namedtuple
from typing import cast

import z3
from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("24"))
data = puzzle.input_data
lines = data.splitlines()

Vec3 = namedtuple("Vec3", ["x", "y", "z"])


def to_k(dx: int, dy: int) -> float:
    return dy / dx


def to_m(x: int, y: int, k: int) -> float:
    return y - x * k


hss: list[tuple[Vec3, Vec3]] = []
for line in lines:
    x, y, z, dx, dy, dz = ints(line)
    hss.append((Vec3(x, y, z), Vec3(dx, dy, dz)))


solver = z3.Solver()

px, py, pz, dx, dy, dz = z3.Ints("px py pz dx dy dz")

for i, (p, d) in enumerate(hss):
    t = z3.Int(f"t_{i}")
    solver.add(px + dx * t == p.x + d.x * t)
    solver.add(py + dy * t == p.y + d.y * t)
    solver.add(pz + dz * t == p.z + d.z * t)

solver.check()
m = solver.model()
px, py, pz = (cast(z3.IntNumRef, m[v]).as_long() for v in (px, py, pz))

puzzle.answer_b = px + py + pz
