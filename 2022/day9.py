# 9   00:11:10   411      0   00:19:38   374      0
# I'm happy with this time

from aocd.models import Puzzle
from numpy import sign

puzzle = Puzzle(2022, int("09"))
id = puzzle.input_data


def move_rope(
    rope: list[tuple[int, int]],
    direction: str,
    amount: int,
    rope_tail_visited: set[tuple[int, int]],
):
    dx, dy = {"D": (0, 1), "U": (0, -1), "L": (-1, 0), "R": (1, 0)}[direction]

    for _ in range(amount):
        hx, hy = rope[0]
        rope[0] = hx + dx, hy + dy

        move_tail(rope)

        rope_tail_visited.add(rope[-1])


def move_tail(rope: list[tuple[int, int]]):
    for tail_i in range(1, len(rope)):
        hx, hy = rope[tail_i - 1]
        tx, ty = rope[tail_i]

        if hx == tx and abs(hy - ty) == 2:
            ty += sign(hy - ty)
        elif hy == ty and abs(hx - tx) == 2:
            tx += sign(hx - tx)
        elif abs(hx - tx) + abs(hy - ty) >= 3:
            tx += sign(hx - tx)
            ty += sign(hy - ty)

        rope[tail_i] = tx, ty


rope_a = [(0, 0), (0, 0)]
rope_b = [(0, 0) for _ in range(10)]
tail_a_visited = {rope_a[-1]}
tail_b_visited = {rope_b[-1]}

for line in id.splitlines():
    direction, amount_str = line.split()
    amount = int(amount_str)

    move_rope(rope_a, direction, amount, tail_a_visited)
    move_rope(rope_b, direction, amount, tail_b_visited)


puzzle.answer_a = len(tail_a_visited)
puzzle.answer_b = len(tail_b_visited)
