# 2   00:05:34   578      0   00:10:22   851      0

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("02"))
id = puzzle.input_data


# Main code
def mod(a: int, b: int) -> int:
    return ((a % b) + b) % b


score_a = 0
score_b = 0

for line in id.splitlines():
    a, b_ = line.split(" ")
    b = {"X": "A", "Y": "B", "Z": "C"}[b_]

    # score_a
    # A => 1, B => 2, C => 3
    score_a += ord(b) - ord("A") + 1

    # Tie if they are the same
    if a == b:
        score_a += 3
    # Win if b is the next character in the sequence [A, B, C] from a
    elif b == chr(mod(ord(a) - ord("A") + 1, 3) + ord("A")):
        score_a += 6

    # score_b
    result_score, move_delta = {"A": (0, 2), "B": (3, 0), "C": (6, 1)}[b]
    # A => loss and use next next move, B => tie and same, C => win and use next
    score_b += result_score + (mod(ord(a) - ord("A") + move_delta, 3) + 1)

puzzle.answer_a = score_a
puzzle.answer_b = score_b
