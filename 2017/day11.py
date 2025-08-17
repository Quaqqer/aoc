from aocd.models import Puzzle
from util import Vec3

puzzle = Puzzle(2017, int("11"))

data = puzzle.input_data

N = Vec3(1, 0, -1)
S = -N
NE = Vec3(0, 1, -1)
SW = -NE
NW = Vec3(1, -1, 0)
SE = -NW

moves = {"n": N, "s": S, "ne": NE, "nw": NW, "se": SE, "sw": SW}


def dist(a: Vec3[int], b: Vec3[int]) -> int:
    d = a - b
    return (abs(d.x) + abs(d.y) + abs(d.z)) // 2


pos = Vec3(0, 0, 0)
max_dist = 0
for move in data.split(","):
    delta = moves[move]
    pos += delta
    max_dist = max(max_dist, dist(Vec3(0, 0, 0), pos))


puzzle.answer_a = dist(Vec3(0, 0, 0), pos)
puzzle.answer_b = max_dist
