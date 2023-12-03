#   3   00:11:46   442      0   00:34:34  1895      0
# part 2 😴

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("03"))
data = puzzle.input_data
board = data.splitlines()

ys = len(board)
xs = len(board[0])


def find_num(x: int, y: int) -> tuple[int, int] | None:
    if not board[y][x].isdigit():
        return None

    sx = x
    while 0 <= sx - 1 and board[y][sx - 1].isdigit():
        sx -= 1

    while x + 1 < xs and board[y][x + 1].isdigit():
        x += 1

    return (sx, x)


symbols = {c for line in board for c in line if not c.isdigit() and c != "."}

part_ranges: set[tuple[int, tuple[int, int]]] = set()
gear_ratios = 0

for x in range(xs):
    for y in range(ys):
        c = board[y][x]
        # Part 1
        if c.isdigit():
            num = find_num(x, y)
            if num is not None:
                sx, ex = num
                ynum = y, num

                if ynum not in part_ranges:
                    for xx in range(sx - 1, ex + 2):
                        for dy in [-1, 0, 1]:
                            yy = y + dy
                            if (
                                0 <= xx < xs
                                and 0 <= yy < ys
                                and board[yy][xx] in symbols
                            ):
                                part_ranges.add(ynum)
        # Part 2
        if c == "*":
            used_coords = set()
            parts = []
            for dy in [-1, 0, 1]:
                for dx in [-1, 0, 1]:
                    xx, yy = x + dx, y + dy
                    if not (0 < xx < xs and 0 <= yy < ys) or (xx, yy) in used_coords:
                        continue
                    num = find_num(xx, yy)
                    if num is not None:
                        sx, ex = num
                        used_coords |= {(xx, yy) for xx in range(sx, ex + 1)}
                        parts.append(int(board[yy][sx : ex + 1]))
            match parts:
                case [p1, p2]:
                    gear_ratios += p1 * p2

part_numbers = sum(int(board[y][sx : ex + 1]) for y, (sx, ex) in part_ranges)
puzzle.answer_a = part_numbers
puzzle.answer_b = gear_ratios
