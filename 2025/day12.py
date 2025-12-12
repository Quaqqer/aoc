# 12   01:10:21   01:10:25
# Why Eric? Why couldn't it be an actual problem instead of just summing areas?

# pyright: basic


from aocd.models import Puzzle
from util import ints

puzzle = Puzzle(2025, int("12"))


def solve(input: str):
    *shapes, lines = input.split("\n\n")
    shape = [sum(c == "#" for c in shape) for shape in shapes]
    tot = 0
    for line in lines.splitlines():
        w, h, *counts = ints(line)
        tot += w * h >= sum(s * c for s, c in zip(shape, counts))
    return tot


puzzle.answer_a = solve(puzzle.input_data)

# def parse(input: str):
#     *shapes_s, lines_s = input.split("\n\n")
#
#     shapes: list[Grid[str]] = []
#     for shape_s in shapes_s:
#         g = Grid.from_lines("\n".join(shape_s.splitlines()[1:]))
#         shapes.append(g)
#
#     lines = lines_s.splitlines()
#     pzs: list[tuple[int, int, list[int]]] = []
#     for line in lines:
#         width, height, *count = ints(line)
#         pzs.append((width, height, count))
#
#     return shapes, pzs
#
#
# def solve_for_real(
#     shapes: list[Grid], width: int, height: int, counts: list[int]
# ) -> bool:
#     """Doesn't even work, because it doesn't flip them."""
#     solver = z3.Solver()
#
#     shape_pos_vars: defaultdict[tuple[int, int], list[Any]] = defaultdict(list)
#     shape_vars: list[list[Any]] = [[] for _ in range(len(counts))]
#
#     for i, shape in enumerate(shapes):
#         s = shape
#         for rotation in range(4):
#             s = s.rotate_right()
#
#             for x in range(width):
#                 for y in range(height):
#                     if any(
#                         s.map_with_pos(
#                             lambda pos, v: v == "#"
#                             and (x + pos[0] >= width or y + pos[1] >= height)
#                         ).values()
#                     ):
#                         continue
#
#                     s_v = z3.Bool(f"shape_{i}_{x}_{y}_{rotation}")
#                     for dx, dy in s:
#                         if s[dx, dy] == "#":
#                             shape_pos_vars[x + dx, y + dy].append(s_v)
#                     shape_vars[i].append(s_v)
#
#     for vars in shape_pos_vars.values():
#         solver.add(z3.Sum(vars) < 2)
#
#     for i, count in enumerate(counts):
#         solver.add(z3.Sum(shape_vars[i]) == count)
#
#     check = solver.check()
#
#     return str(check) == "sat"
#
#
# def solve(input: str):
#     shapes, pzs = parse(input)
#     return sum(solve_for_real(shapes, *pz) for pz in pzs)
#
#
# cpp(solve(puzzle.input_data))
