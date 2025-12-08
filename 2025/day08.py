# 8   00:26:03   00:27:23
# Had a really hard time understanding what Eric meant in part 1 today 😅

# pyright: basic

from collections import Counter

from aocd.models import Puzzle
from util import Heap, UnionFind, Vec3, ints

puzzle = Puzzle(2025, int("08"))

type Box = tuple[int, int, int]


def dist(a: Box, b: Box) -> float:
    ax, ay, az = a
    bx, by, bz = b
    d = (Vec3(ax, ay, az) - Vec3(bx, by, bz)).length()
    return d


def parse(input: str) -> list[Box]:
    boxes = []
    for line in input.splitlines():
        x, y, z = ints(line)
        box = x, y, z
        boxes.append(box)
    return boxes


def solve_a(input: str):
    boxes = parse(input)

    q = Heap[tuple[float, Box, Box]]()
    uf = UnionFind[Box]()

    for ai, a in enumerate(boxes):
        for b in boxes[ai + 1 :]:
            ax, ay, az = a
            bx, by, bz = b
            d = (Vec3(ax, ay, az) - Vec3(bx, by, bz)).length()
            q.push((d, a, b))

    for _ in range(1000):
        _, a, b = q.pop()
        uf.union(a, b)

    counts = Counter([uf.find(box) for box in boxes])
    a, b, c, *_ = sorted(counts.values(), reverse=True)
    return a * b * c


def solve_b(input: str):
    boxes = parse(input)

    q = Heap[tuple[float, Box, Box]]()
    uf = UnionFind[Box]()

    for ai, a in enumerate(boxes):
        for b in boxes[ai + 1 :]:
            q.push((dist(a, b), a, b))

    s = 0
    while q:
        _, a, b = q.pop()
        if uf.find(a) != uf.find(b):
            uf.union(a, b)
            s = a[0] * b[0]

    return s


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
