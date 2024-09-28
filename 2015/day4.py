import hashlib

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("04"))
data: str = puzzle.input_data


def mine(prefix: str):
    i = 0
    while True:
        if hashlib.md5((data + str(i)).encode("utf-8")).hexdigest().startswith(prefix):
            return i
        i += 1


puzzle.answer_a = mine("00000")
puzzle.answer_b = mine("000000")
