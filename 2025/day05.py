# 5   00:03:35   00:17:20
# A bit tricky part 2 today, but I'm proud to report that I never submitted any
# bad solutions. 🫡

# pyright: basic

from aocd.models import Puzzle
from util import Range

puzzle = Puzzle(2025, int("05"))


def parse(input: str) -> tuple[list[Range], list[int]]:
    ranges_s, nums_s = input.split("\n\n")

    ranges: list[Range] = []
    for line in ranges_s.splitlines():
        a, b = line.split("-")
        ranges.append(Range(int(a), int(b) + 1))
    nums = list(map(int, nums_s.split()))
    return ranges, nums


def solve_a(input: str):
    ranges, nums = parse(input)

    return sum(any(n in range for range in ranges) for n in nums)


def solve_b(input: str):
    ranges, _ = parse(input)

    overlaps: list[tuple[Range, int]] = []

    for range in ranges:
        new_overlaps = [(range, 1)]
        for prev_overlap, m in overlaps:
            if (overlap := prev_overlap.overlap(range)) is not None:
                new_overlaps.append((overlap, m * -1))
        overlaps += new_overlaps
    return sum(range.len() * m for range, m in overlaps)


puzzle.answer_a = solve_a(puzzle.input_data)
puzzle.answer_b = solve_b(puzzle.input_data)
