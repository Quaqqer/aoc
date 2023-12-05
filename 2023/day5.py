from __future__ import annotations

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("05"))
data = puzzle.input_data


class Range:
    """Range [start, end)"""

    def __init__(self, start: int, end: int):
        assert start < end
        self.start = start
        self.end = end

    def overlap(self, other: Range) -> Range | None:
        ostart = max(self.start, other.start)
        oend = min(self.end, other.end)

        if ostart < oend:
            return Range(ostart, oend)

    def __add__(self, v: int) -> Range:
        return Range(self.start + v, self.end + v)

    def __repr__(self) -> str:
        return f"[{self.start}, {self.end})"


seeds, *mapss = data.split("\n\n")
seeds = list(map(int, seeds.split(": ")[1].split()))
maps: list[list[tuple[int, Range]]] = []
for i, m in enumerate((m.splitlines()[1:] for m in mapss)):
    maps.append([])

    for l in m:
        dest, start, length = map(int, l.split())
        maps[i].append((dest, Range(start, start + length)))

    maps[i].sort(key=lambda v: v[1].start)


def overlaps(m: int, r: Range) -> list[Range]:
    """Recursively find overlaps for ranges in the maps."""
    if m == len(maps):
        return [r]

    rs = []
    last_end = r.start
    for dest, r2 in maps[m]:
        overlap = r.overlap(r2)

        if overlap:
            if last_end < overlap.start:
                rs += overlaps(m + 1, Range(last_end, overlap.start))

            delta = dest - r2.start
            dest_range = overlap + delta
            rs += overlaps(m + 1, dest_range)
            last_end = overlap.end

    if last_end < r.end:
        rs += overlaps(m + 1, Range(last_end, r.end))

    return rs


puzzle.answer_a = min(
    overlap.start for seed in seeds for overlap in overlaps(0, Range(seed, seed + 1))
)
puzzle.answer_b = min(
    overlap.start
    for start, length in zip(seeds[0::2], seeds[1::2])
    for overlap in overlaps(0, Range(start, start + length))
)
