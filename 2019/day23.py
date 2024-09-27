# Solved it in less than 15 minutes
# 1. I opened the problem ~12:11:20 according to firefox
# 2. I submitted correct answer for part 1 at 12:19:59 according to aocd => ~8 minutes and 39 seconds
# 3. I submitted correct answer for part 2 at 12:26:02 according to aocd => ~14 minutes and 42 seconds
#
# If this was december I would place top 50 in part 1 and top 38 in part 2, cool stuff

from collections import deque

import util
from aocd.models import Puzzle
from intpc import IntPC

puzzle = Puzzle(2019, int("23"))
data = puzzle.input_data
program = util.ints(data.split(","))


def solve_a() -> int:
    pcs = [IntPC(program, [i]) for i in range(50)]

    packets: dict[int, deque[tuple[int, int]]] = {i: deque() for i in range(50)}

    while True:
        for i in range(50):
            pc = pcs[i]
            input = None if len(packets[i]) == 0 else packets[i].popleft()

            if input is None:
                pc.queue_input([-1])
            else:
                pc.queue_input(input)
            pc.run()

            for to, x, y in util.chunk(pc.consume_outputs(), 3):
                if to == 255:
                    return y

                packets[to].append((x, y))


def solve_b() -> int:
    pcs = [IntPC(program, [i]) for i in range(50)]

    packets: dict[int, deque[tuple[int, int]]] = {i: deque() for i in range(50)}

    nat_x, nat_y = -1, -1

    sent_ys: set[int] = set()

    while True:
        idle = all(len(q) == 0 for q in packets.values())

        if idle:
            packets[0].append((nat_x, nat_y))

            if nat_y in sent_ys:
                return nat_y

            sent_ys |= {nat_y}

        for i in range(50):
            pc = pcs[i]
            input = None if len(packets[i]) == 0 else packets[i].popleft()

            if input is None:
                pc.queue_input([-1])
            else:
                pc.queue_input(input)
            pc.run()

            for to, x, y in util.chunk(pc.consume_outputs(), 3):
                if to == 255:
                    nat_x, nat_y = x, y
                else:
                    packets[to].append((x, y))


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
