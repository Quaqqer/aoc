import util
from aocd.models import Puzzle
from intpc import IntPC

puzzle = Puzzle(2019, int("21"))
data = puzzle.input_data
program = util.ints(data.split(","))


intpc = IntPC(program)

# @
# ##########
#  ABCDEFGHI

# Jump if A, B or C is a hole iff E or H is ground
instructions_a = """OR A J
AND B J
AND C J
NOT J J
AND D J
WALK
"""

instructions_b = """OR A J
AND B J
AND C J
NOT J J
AND D J
OR E T
OR H T
AND T J
RUN
"""

puzzle.answer_a = IntPC(program).io(instructions_a)[-1]
puzzle.answer_b = IntPC(program).io(instructions_b)[-1]
