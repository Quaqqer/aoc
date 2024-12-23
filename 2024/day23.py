# 23   00:13:40  1500      0   00:18:01   636      0
# Networkx to the rescue. I was still pretty slow though.

import itertools
from collections import defaultdict

import networkx
from aocd.models import Puzzle

puzzle = Puzzle(2024, int("23"))

data = puzzle.input_data

edges = defaultdict(set)
for line in data.splitlines():
    a, b = line.split("-")
    edges[a].add(b)
    edges[b].add(a)


def find_triangles(edges: dict[str, set[str]]):
    triangles = set()
    for a in edges:
        for b, c in itertools.combinations(edges[a], 2):
            if b in edges[c]:
                triangles.add(tuple(sorted([a, b, c])))
    return triangles


g = networkx.Graph(edges)


puzzle.answer_a = sum(any(n.startswith("t") for n in c) for c in find_triangles(edges))
_, largest_clique = max((len(c), c) for c in networkx.find_cliques(g))
puzzle.answer_b = ",".join(sorted(largest_clique))
