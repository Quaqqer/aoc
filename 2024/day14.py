# 14   00:09:20   515      0   00:25:33   439      0
# Tricky! Using a flood fill was a good idea to find the christmas tree, but
# there are a lot of different parameters and you have to guess.

import itertools
import math

from aocd.models import Puzzle
from util import Vec2, ints

puzzle = Puzzle(2024, int("14"))

data = puzzle.input_data

lines = data.splitlines()

W = 101
H = 103


def region(s: set[Vec2], p: Vec2, visited) -> int:
    visited.add(p)

    amt = 1

    for delta in Vec2.delta_4():
        n = p + delta
        if n in s and n not in visited:
            amt += region(s, n, visited)

    return amt


def solve_a(robots: list[tuple[Vec2, Vec2]]):
    positions = [(p + 100 * v) % Vec2(W, H) for p, v in robots]
    quads = [0, 0, 0, 0]
    for p in positions:
        if p.x == W // 2 or p.y == H // 2:
            continue
        quad = (p.x < W // 2) << 1 | (p.y < H // 2)
        quads[quad] += 1
    return math.prod(quads)


def solve_b(robots: list[tuple[Vec2, Vec2]]):
    for i in itertools.count(1):
        robots = [(Vec2((p.x + v.x) % W, (p.y + v.y) % H), v) for p, v in robots]
        positions = {p for p, _ in robots}
        visited = set()
        for r in positions:
            if region(positions, r, visited) > 20:
                return i


robots: list[tuple[Vec2, Vec2]] = []
for line in lines:
    px, py, vx, vy = ints(line)
    pos = Vec2(px, py)
    vel = Vec2(vx, vy)
    robots.append((pos, vel))

puzzle.answer_a = solve_a(robots)
puzzle.answer_b = solve_b(robots)
