# 24   00:33:02   563      0   02:12:28   940      0
# I have succumbed to Z3. It kind of feels like cheating, but I tried solving
# it and I would sit here all day ¯\_(ツ)_/¯, and in Sweden we celebrate
# christmas eve and not christmas day. If there is another way I would love to
# solve it myself. Merry christmas!

from typing import NamedTuple, cast

import z3
from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("24"))
data = puzzle.input_data
lines = data.splitlines()


class Vec3(NamedTuple):
    x: int
    y: int
    z: int


hss: list[tuple[Vec3, Vec3]] = []
for line in lines:
    x, y, z, dx, dy, dz = ints(line)
    hss.append((Vec3(x, y, z), Vec3(dx, dy, dz)))

s = 0
for i in range(len(hss)):
    for j in range(i + 1, len(hss)):
        ap, ad = hss[i]
        apx, apy, apz = ap
        adx, ady, adz = ad
        bp, bd = hss[j]
        bpx, bpy, bpz = bp
        bdx, bdy, bdz = bd

        ak = ady / adx
        bk = bdy / bdx
        am = apy - apx * ak
        bm = bpy - bpx * bk

        if ak - bk != 0:
            ix = (bm - am) / (ak - bk)
            iy = am + ix * ak

            if (
                (ix - apx) / adx > 0
                and (ix - bpx) / bdx > 0
                and (iy - apy) / ady > 0
                and (iy - bpy) / bdy > 0
                and 200000000000000 <= ix <= 400000000000000
                and 200000000000000 <= iy <= 400000000000000
            ):
                s += 1

puzzle.answer_a = s


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
