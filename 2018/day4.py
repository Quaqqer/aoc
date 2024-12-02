from collections import defaultdict
from datetime import datetime

from aocd.models import Puzzle

puzzle = Puzzle(2018, int("04"))
data = puzzle.input_data
lines = data.splitlines()
lines.sort(key=lambda s: datetime.fromisoformat(s.split("]", 1)[0][1:]))

guard_slept: dict[int, dict[int, int]] = defaultdict(lambda: defaultdict(int))

guard = 0
begin_sleep = 0
for line in lines:
    date = datetime.fromisoformat(line[1:].split("]")[0])

    if "#" in line:
        guard = int(line.split("#", 1)[1].split()[0])
    if "falls" in line:
        begin_sleep = date.minute
    if "wakes" in line:
        end_sleep = date.minute

        for m in range(begin_sleep, end_sleep + 1):
            guard_slept[guard][m] += 1

# Find the worst guard by the minutes he slept
worst_guard = max(guard_slept.keys(), key=lambda g: sum(guard_slept[g].values()))
# Find the worst minute of the worst guard
worst_min = max(
    guard_slept[worst_guard].keys(), key=lambda m: guard_slept[worst_guard][m]
)
puzzle.answer_a = worst_guard * worst_min

# Find the worst combination of guard and minute by taking the amount of minutes the guard spent asleep
worst_guard, worst_min = max(
    ((m, g) for g in guard_slept for m in guard_slept[g]),
    key=lambda mg: guard_slept[mg[1]][mg[0]],
)
puzzle.answer_b = worst_guard * worst_min
