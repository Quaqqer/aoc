# 25   00:34:24   810      0   00:34:25   704      0
# Would have been faster if graphviz would have actually worked, Linux moment..
# Nice to be done, I can go back to sleep! See you next year.

import networkx as nx
from aocd.models import Puzzle

puzzle = Puzzle(2023, int("25"))
data = puzzle.input_data
lines = data.splitlines()

g = nx.Graph()

for line in lines:
    from_, tos = line.split(": ")
    tos = tos.split()

    for to in tos:
        g.add_edge(from_, to)

for e in nx.minimum_edge_cut(g):
    g.remove_edge(*e)

c1, c2 = nx.connected_components(g)

puzzle.answer_a = len(c1) * len(c2)
