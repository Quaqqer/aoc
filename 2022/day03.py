# 3   00:05:15   640      0   00:17:00  2857      0
# part 2 choke?

from functools import reduce
from typing import Iterable, Sequence, TypeVar

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("03"))
id = puzzle.input_data


T = TypeVar("T")


def char_to_prio(c: str):
    if c.islower():
        return ord(c) - ord("a") + 1
    else:
        return ord(c) - ord("A") + 27


def intersection_priority(intersections: Iterable[str]) -> int:
    return min(map(char_to_prio, intersections))


def intersections(iter: Iterable[set[T]]) -> set[T]:
    return reduce(lambda l, r: l.intersection(r), iter)


def split_by_middle(lst: Sequence[T]) -> tuple[Sequence[T], Sequence[T]]:
    n = len(lst)
    return lst[: n // 2], lst[n // 2 :]


def split_every_k(seq: Sequence[T], k: int) -> list[Sequence[T]]:
    return [
        [seq[i + di] for di in range(min(k, len(seq) - i))]
        for i in range(0, len(seq), 3)
    ]


lines = id.splitlines()

answer_a = sum(
    intersection_priority(intersections(map(set, split_by_middle(line))))
    for line in lines
)


groups = [[set(elf) for elf in elves] for elves in split_every_k(lines, 3)]
answer_b = sum(intersection_priority(intersections(group)) for group in groups)

puzzle.answer_a = answer_a
puzzle.answer_b = answer_b
