# I LOVE MATH!!!!!!!!!!!!!! It's amazing how you can simplify this assignment
# if you know the proper math, which I don't, but with some help of from my
# friends we came up with this

from __future__ import annotations

from aocd.models import Puzzle
from util import Mat

puzzle = Puzzle(2019, int("22"))
data = puzzle.input_data
lines = data.splitlines()


A_CARDS = 10_007
B_CARDS = 119_315_717_514_047
B_REPETITIONS = 101_741_582_076_661

m = Mat([[1, 0], [0, 1]])

for line in data.splitlines():
    match line.split():
        case "cut", n:
            m @= Mat([[1, 0], [-int(n), 1]])
        case "deal", "with", "increment", n:
            m @= Mat([[int(n), 0], [0, 1]])
        case "deal", "into", "new", "stack":
            m @= Mat([[-1, 0], [-1, 1]])


puzzle.answer_a = (Mat([[2019, 1]]) @ m)[0, 0] % A_CARDS

# Since part two is to find the new card at index 2020 instead of where 2019
# ended up we take the modular inverse of the matrix, it's like magic.
m2 = m.modular_inverse(B_CARDS)
# And raise it to the power of the amount of repetitions
m2 = m2.pow_mod(B_REPETITIONS, B_CARDS)
puzzle.answer_b = (Mat([[2020, 1]]) @ m2)[0, 0] % B_CARDS
