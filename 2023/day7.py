# 7   00:52:46  5903      0   00:59:18  3613      0
# overslept and sleep deprivation, typical :/

from collections import Counter
from collections.abc import Callable

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("07"))
data = puzzle.input_data
lines = data.splitlines()

cards = "1234556789TJQKA"
cards2 = "J1234556789TQKA"


def hand_type(hand: str, p2: bool) -> int:
    counts = Counter(hand)

    js = counts.pop("J") if p2 and "J" in counts else 0

    match sorted(counts.values(), reverse=True) + [0, 0]:
        case [x, *_] if x + js == 5:
            return 6
        case [x, *_] if x + js == 4:
            return 5
        case [x, y, *_] if x + js >= 3 and y + js >= 2 and x + y + js >= 5:
            return 4
        case [x, *_] if x + js == 3:
            return 3
        case [x, y, *_] if x + js >= 2 and y + js >= 2 and x + y + js >= 4:
            return 2
        case [x, *_] if x + js == 2:
            return 1
        case _:
            return 0


def comparator(p2: bool) -> Callable[[tuple[str, int]], tuple[int, ...]]:
    def compare(hand):
        return (hand_type(hand, p2), *((cards2 if p2 else cards).find(c) for c in hand))

    return lambda hand_val: compare(hand_val[0])


hand_vals = []
for line in lines:
    hand, score = line.split()
    hand_vals.append((hand, int(score)))


puzzle.answer_a = sum(
    (i + 1) * v for i, (_, v) in enumerate(sorted(hand_vals, key=comparator(False)))
)
puzzle.answer_b = sum(
    (i + 1) * v for i, (_, v) in enumerate(sorted(hand_vals, key=comparator(True)))
)
