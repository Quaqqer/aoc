import functools

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("17"))
data = puzzle.input_data

containers = list(map(int, data.split()))


@functools.cache
def container_combinations(space: int, i=0) -> int:
    if space == 0:
        return 1

    if i == len(containers):
        return 0

    return container_combinations(
        space - containers[i], i + 1
    ) + container_combinations(space, i + 1)


@functools.cache
def min_containers(space: int, i=0) -> int | None:
    if space == 0:
        return 0

    if i == len(containers):
        return None

    ans: list[int] = []

    if (used := min_containers(space - containers[i], i + 1)) is not None:
        ans.append(used + 1)

    if (unused := min_containers(space, i + 1)) is not None:
        ans.append(unused)

    return None if ans == [] else min(ans)


puzzle.answer_a = container_combinations(150)
puzzle.answer_b = min_containers(150)
