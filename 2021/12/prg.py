#!/usr/bin/env python3
from collections import defaultdict

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 12)
lines = puzzle.input_data.split("\n")


# Main code
def dfs(path, connections, visited, paths, is_double):
    """
    Search through all connections.

    :param path list[str]: Current path, starts with ["start"]
    :param connections dict[str, list[str]]: Connections for each pos.
    :param visited dict[str, int]: The amount of visitations per node.
    :param paths list[str]: Output dictionary.
    :param is_double bool: If a small cave has been visited double already.
    """

    pos = path[-1]
    if pos == "end":
        paths.append(path.copy())
    else:
        for c in connections[pos]:
            c: str
            if c.isupper():
                path.append(c)
                dfs(path, connections, visited, paths, is_double)
            elif visited[c] == 0:
                visited[c] += 1
                path.append(c)
                dfs(path, connections, visited, paths, is_double)
                visited[c] -= 1
            elif visited[c] == 1 and not is_double:
                visited[c] += 1
                path.append(c)
                dfs(path, connections, visited, paths, True)
                visited[c] -= 1
    path.pop()


connections = defaultdict(list)
for line in lines:
    a, b = tuple(line.split("-"))
    connections[a].append(b)
    connections[b].append(a)


visited = defaultdict(int)
visited["start"] = 2
paths = []
dfs(["start"], connections, visited, paths, True)
silver = len(paths)

visited = defaultdict(int)
visited["start"] = 2
paths = []
dfs(["start"], connections, visited, paths, False)
gold = len(paths)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
