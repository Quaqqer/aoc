from aocd.models import Puzzle

from util import *

puzzle = Puzzle(2023, int("05"))
data = puzzle.input_data

seeds, *mapss = data.split("\n\n")
seeds = list(map(int, seeds.split(": ")[1].split()))
maps: list[list[tuple[int, int, int]]] = []
for i, m in enumerate((m.splitlines()[1:] for m in mapss)):
    maps.append([])
    for l in m:
        dest, src, rang = map(int, l.split())
        maps[i].append((dest, src, src + rang - 1))

    maps[i].sort(key=lambda v: v[1])


def overlaps(m: int, r: tuple[int, int]) -> list[tuple[int, int]]:
    if m == len(maps):
        return [r]

    start, end = r

    os = []
    last_end = r[0]
    for dest, mstart, mend in maps[m]:
        ostart, oend = max(start, mstart), min(end, mend)

        if start <= ostart and oend <= end and ostart <= oend:
            if last_end < ostart - 1:
                os += overlaps(m + 1, (last_end, ostart - 1))

            delta = dest - mstart
            dstart = ostart + delta
            dend = oend + delta
            last_end = oend + 1
            os += overlaps(m + 1, (dstart, dend))

    if last_end <= r[1]:
        os += overlaps(m + 1, (last_end, r[1]))

    return os


def best_answer(ranges: Iterable[tuple[int, int]]):
    return min(
        list(zip(*(o for start, end in ranges for o in overlaps(0, (start, end)))))[0]
    )


puzzle.answer_a = best_answer((seed, seed) for seed in seeds)
puzzle.answer_b = best_answer(
    (start, start + rang - 1) for start, rang in zip(seeds[0::2], seeds[1::2])
)
