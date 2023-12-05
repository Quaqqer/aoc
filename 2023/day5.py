from __future__ import annotations

from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("05"))
data = puzzle.input_data

seeds, *mapss = data.split("\n\n")
mapss = [map_.splitlines()[1:] for map_ in mapss]

seeds = [int(v) for v in seeds.split(": ")[1].split()]
maps = [
    sorted(
        (
            (dest, Range(start, start + length))
            for dest, start, length in (map(int, l.split()) for l in map_)
        ),
        key=lambda v: v[1].start,
    )
    for map_ in mapss
]


def overlaps(i: int, r: Range) -> list[Range]:
    """Recursively find overlaps for ranges in the maps."""
    if i == len(maps):
        return [r]

    rs = []
    last_end = r.start
    for dest, r2 in maps[i]:
        overlap = r.overlap(r2)

        if overlap:
            if last_end < overlap.start:
                rs += overlaps(i + 1, Range(last_end, overlap.start))

            delta = dest - r2.start
            dest_range = overlap + delta
            rs += overlaps(i + 1, dest_range)
            last_end = overlap.end

    if last_end < r.end:
        rs += overlaps(i + 1, Range(last_end, r.end))

    return rs


puzzle.answer_a = min(
    overlap.start for seed in seeds for overlap in overlaps(0, Range(seed, seed + 1))
)

puzzle.answer_b = min(
    overlap.start
    for start, length in zip(seeds[0::2], seeds[1::2])
    for overlap in overlaps(0, Range(start, start + length))
)
