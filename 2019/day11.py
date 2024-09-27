import util
from aocd.models import Puzzle
from intpc import IntPC, State

puzzle = Puzzle(2019, int("11"))
data = puzzle.input_data
program = util.ints(data.split(","))


x, y = 0, 0
dx, dy = 0, -1
painted = set()
whites = set()


def rot_r():
    global dx, dy
    dx, dy = -dy, dx


def rot_l():
    global dx, dy
    dx, dy = dy, -dx


intpc = IntPC(program)
while True:
    intpc.run([1 if (x, y) in whites else 0])

    if intpc.state == State.HALTED:
        break

    *_, paint, turn = intpc.consume_outputs()
    painted.add((x, y))
    if paint == 1:
        whites.add((x, y))
    else:
        if (x, y) in whites:
            whites.remove((x, y))

    if turn == 1:
        rot_r()
    else:
        rot_l()

    x, y = x + dx, y + dy

puzzle.answer_a = len(painted)

x, y = 0, 0
dx, dy = 0, -1
whites = {(0, 0)}
painted = set()

intpc = IntPC(program)
while True:
    intpc.run([1 if (x, y) in whites else 0])

    if intpc.state == State.HALTED:
        break

    *_, paint, turn = intpc.outputs
    painted.add((x, y))
    if paint == 1:
        whites.add((x, y))
    else:
        if (x, y) in whites:
            whites.remove((x, y))

    if turn == 1:
        rot_r()
    else:
        rot_l()

    x, y = x + dx, y + dy


util.print_set_grid(whites)

print("What letters do you see?")
ans = input("> ")
assert len(ans) == 8
puzzle.answer_b = ans
