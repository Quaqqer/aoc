# Part 1: 7:11
# Part 2: 12:21

from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2018, int("08"))
data = puzzle.input_data
lines = data.splitlines()

type Node = tuple[list[Node], list[int]]


def parse_node(i: int, l: list[int]) -> tuple[int, Node]:
    cs = l[i]
    ms = l[i + 1]
    i += 2
    children: list[Node] = []
    metadatas: list[int] = []
    for _ in range(cs):
        i, child = parse_node(i, l)
        children += [child]
    for _ in range(ms):
        metadatas += [l[i]]
        i += 1
    return i, (children, metadatas)


_, n = parse_node(0, ints(data))


def value_a(n: Node) -> int:
    cs, ms = n
    return sum(value_a(c) for c in cs) + sum(ms)


def value_b(n: Node) -> int:
    match n:
        case ([], ms):
            return sum(ms)
        case (cs, ms):
            return sum(value_b(cs[m - 1]) for m in ms if 0 <= m - 1 < len(cs))


puzzle.answer_a = value_a(n)
puzzle.answer_b = value_b(n)
