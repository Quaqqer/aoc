from aocd.models import Puzzle

from util import ints

puzzle = Puzzle(2023, int("04"))
data = puzzle.input_data
lines = data.splitlines()


def scratch(winning: set[int], yours: list[int]) -> int:
    return sum(1 for y in yours if y in winning)


cards: list[tuple[set[int], list[int]]] = []
cards_amt = [1 for _ in range(len(lines))]

for line in lines:
    winning, yours = line.split(": ")[1].split("|")
    winning = set(ints(winning))
    yours = ints(yours)
    cards.append((winning, yours))


for i, card in enumerate(cards):
    wins = scratch(*card)
    for j in range(wins):
        cards_amt[i + 1 + j] += cards_amt[i]

puzzle.answer_a = sum(2 ** scratch(winning, yours) // 2 for winning, yours in cards)
puzzle.answer_b = sum(cards_amt)
