# 20   00:12:11   211      0   00:46:33   923      0
# Good time for part 1, not so good for part 2. I accidentally used a normal
# list instead of a deque for my BFS, I'm not used to doing BFSs anymore 😅

from collections import Counter, deque

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("20"))

data = puzzle.input_data


def search(start: tuple[int, int], g: list[list[str]]) -> dict[tuple[int, int], int]:
    """Search for costs from start"""

    q = deque([(0, start)])
    costs = {}
    while q:
        cost, pos = q.popleft()
        x, y = pos

        if pos in costs:
            continue
        costs[pos] = cost

        for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            n = x + dx, y + dy
            nx, ny = n
            if g[ny][nx] == ".":
                q.append((cost + 1, n))
    return costs


def search_cheats(
    start: tuple[int, int], costs: dict[tuple[int, int], int], max_cost: int
) -> Counter[int]:
    cheats = Counter()

    x, y = start
    for dx in range(-max_cost, max_cost + 1):
        for dy in range(-max_cost, max_cost + 1):
            dist = abs(dx) + abs(dy)
            if dist > max_cost:
                continue

            end = x + dx, y + dy
            end_x, end_y = end
            if 0 <= end_x < W and 0 <= end_y < H and g[end_y][end_x] == ".":
                saved = costs[end] - costs[start] - dist
                cheats[saved] += 1

    return cheats


g = [list(line) for line in data.splitlines()]
H = len(g)
W = len(g[0])
[start] = [(x, y) for y in range(H) for x in range(W) if g[y][x] == "S"]
[end] = [(x, y) for y in range(H) for x in range(W) if g[y][x] == "E"]
start_x, start_y = start
end_x, end_y = end
g[start_y][start_x] = "."
g[end_y][end_x] = "."

costs = search(start, g)


def solve(max_cost: int):
    cheats = Counter()
    for p in ((x, y) for y in range(H) for x in range(W) if g[y][x] == "."):
        cheats += search_cheats(p, costs, max_cost)
    return sum(n for saved, n in cheats.items() if saved >= 100)


puzzle.answer_a = solve(2)
puzzle.answer_b = solve(20)
