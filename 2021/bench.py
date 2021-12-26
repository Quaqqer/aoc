import importlib
import os
import time as t

times: list[tuple[int, float]] = []

for day in range(1, 25 + 1):
    pypath = f"day{day}.py"
    if os.path.exists(pypath):
        start = t.time()
        importlib.import_module(pypath.removesuffix(".py"))
        end = t.time()
        times.append((day, end - start))

total = sum(map(lambda v: v[1], times))
print(f"Total time taken: {total:.3f} seconds")
for day, time in times:
    print(
        f"Day {day} took {time:.3} seconds, "
        + f"{100*time/total:.2f}% of the total time taken."
    )
