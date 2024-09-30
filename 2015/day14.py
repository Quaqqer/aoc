import re
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("14"))
data = puzzle.input_data
lines = data.splitlines()

reindeers: dict[str, tuple[int, int, int]] = {}

for line in lines:
    m = re.match(
        r"(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds\.",
        line,
    )
    assert m is not None
    reindeer, velocity, fly_time, rest = m.groups()
    reindeers[reindeer] = int(velocity), int(fly_time), int(rest)


def distance_after_time(velocity: int, fly_time: int, rest: int, time: int) -> int:
    return (velocity * fly_time) * (time // (fly_time + rest)) + velocity * min(
        fly_time, time % (fly_time + rest)
    )


puzzle.answer_a = max(distance_after_time(*r, 2503) for r in reindeers.values())

scores: dict[str, int] = defaultdict(int)

for s in range(1, 2503):
    s_distances = {
        name: distance_after_time(*stats, s) for name, stats in reindeers.items()
    }

    best_dist = max(s_distances.values())

    for name in [r for r, d in s_distances.items() if d == best_dist]:
        scores[name] += 1

puzzle.answer_b = max(scores.values())
