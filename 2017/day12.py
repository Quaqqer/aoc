import util
from aocd.models import Puzzle

puzzle = Puzzle(2017, int("12"))

data = puzzle.input_data

lines = data.splitlines()

uf = util.UnionFind()

nodes = set()
for line in lines:
    l, rs = line.split(" <-> ")
    rs = rs.split(", ")

    for r in rs:
        uf.union(l, r)

    nodes |= {l} | set(rs)

s = 0
for node in nodes:
    if uf.find(node) == uf.find("0"):
        s += 1


puzzle.answer_a = s
puzzle.answer_b = len({uf.find(node) for node in nodes})
