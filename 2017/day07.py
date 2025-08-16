import functools
from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2017, int("07"))

data = puzzle.input_data

g: dict[str, list[str]] = {}
weights: dict[str, int] = {}
all_children: set[str] = set()

for line in data.splitlines():
    if " -> " in line:
        parent, children = line.split(" -> ")
        parent, n = parent.split(" ")
        children = children.split(", ")
        n = int(n[1:-1])

        weights[parent] = n
        g[parent] = children
        all_children |= set(children)
    else:
        parent, n = line.split(" ")
        n = int(n[1:-1])

        weights[parent] = n

root = next(node for node in g if node not in all_children)


@functools.cache
def weight(node: str) -> int:
    w = weights[node]
    for child in g.get(node, []):
        w += weight(child)
    return w


def find_correct_weight(node: str) -> int | None:
    children = g.get(node, [])

    # Check all children first, if a child is unbalanced this will be
    # unbalanced too. Solving this won't solve the child, so we must check the
    # child first.
    for child in children:
        ans = find_correct_weight(child)
        if ans is not None:
            return ans

    # Count nodes of a certain weight
    weight_nodes: dict[int, list[str]] = defaultdict(list)
    for c in children:
        weight_nodes[weight(c)].append(c)

    # If there is a weight only one child has, it is inbalanced
    single_weight = [nodes for nodes in weight_nodes.values() if len(nodes) == 1]
    if single_weight:
        [[wrong_child]] = single_weight
        wrong_weight = weight(wrong_child)
        right_weight = next((w for w, c in weight_nodes.items() if len(c) != 1))
        return weights[wrong_child] + right_weight - wrong_weight


puzzle.answer_a = root
puzzle.answer_b = find_correct_weight(root)
