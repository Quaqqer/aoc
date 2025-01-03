# 20   00:12:11   211      0   00:46:33   923      0
# Good time for part 1, not so good for part 2. I accidentally used a normal
# list instead of a deque for my BFS, I'm not used to doing BFSs anymore 😅

from collections import Counter, deque

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("20"))

data = puzzle.input_data


def search(start: Vec2[int], g: Grid[str]) -> dict[Vec2, int]:
    """Search for costs from start"""

    q = deque([(0, start)])
    costs = {}
    while q:
        cost, pos = q.popleft()

        if pos in costs:
            continue
        costs[pos] = cost

        for n in map(Vec2, g.neighbours4(pos.tup)):
            if g[n] == ".":
                q.append((cost + 1, n))
    return costs


def search_cheats(
    start: Vec2[int], costs: dict[Vec2[int], int], max_cost: int
) -> Counter[int]:
    cheats = Counter()

    for dx in range(-max_cost, max_cost + 1):
        for dy in range(-max_cost, max_cost + 1):
            delta = Vec2(dx, dy)
            if delta.manhattan() > max_cost:
                continue

            end = start + Vec2(dx, dy)

            if end in g and g[end] == ".":
                saved = costs[end] - costs[start] - delta.manhattan()
                cheats[saved] += 1

    return cheats


g = Grid.from_lines(data)
[start] = map(Vec2, g.find_value("S"))
[end] = map(Vec2, g.find_value("E"))
g[start] = "."
g[end] = "."

costs = search(start, g)


def solve(max_cost: int):
    cheats = Counter()
    for p in map(Vec2, g.find_value(".")):
        cheats += search_cheats(p, costs, max_cost)
    return sum(n for saved, n in cheats.items() if saved >= 100)


puzzle.answer_a = solve(2)
puzzle.answer_b = solve(20)
