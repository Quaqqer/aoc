#   7   00:52:46  5903      0   00:59:18  3613      0
# overslept and sleep deprivation, typical :/

from functools import cmp_to_key

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("07"))
data = puzzle.input_data
lines = data.splitlines()

cards = "1234556789TJQKA"
cards2 = "J1234556789TQKA"


def c_vals(h: str) -> tuple[int, ...]:
    return tuple(cards.find(c) for c in hand)


def type(hand: str) -> int:
    for card in cards:
        if len([c for c in hand if c == card]) == 5:
            return 7

    for card in cards:
        if len([c for c in hand if c == card]) == 4:
            return 6

    for card1 in cards:
        for card2 in cards:
            if card1 == card2:
                continue

            if (
                len([c for c in hand if c == card1]) == 3
                and len([c for c in hand if c == card2]) == 2
            ):
                return 5

    for card in cards:
        if len([c for c in hand if c == card]) == 3:
            return 4

    for card1 in cards:
        for card2 in cards:
            if card1 == card2:
                continue

            if (
                len([c for c in hand if c == card1]) == 2
                and len([c for c in hand if c == card2]) == 2
            ):
                return 2

    for card in cards:
        if len([c for c in hand if c == card]) == 2:
            return 1

    return 0


def type2(hand: str) -> int:
    for card in cards:
        if len([c for c in hand if c == card or c == "J"]) == 5:
            return 7

    for card in cards:
        if len([c for c in hand if c == card or c == "J"]) == 4:
            return 6

    for card1 in cards:
        for card2 in cards:
            if card1 == card2:
                continue

            c1s = []
            for i, c in enumerate(hand):
                if c == card1 or c == "J":
                    c1s.append(i)

                if len(c1s) == 3:
                    break

            c2s = []
            for i, c in enumerate(hand):
                if i in c1s:
                    continue

                if c == card2 or c == "J":
                    c2s.append(i)

                if len(c2s) == 2:
                    break

            if len(c1s) == 3 and len(c2s) == 2:
                return 5

    for card in cards:
        if len([c for c in hand if c == card or c == "J"]) == 3:
            return 4

    for card1 in cards:
        for card2 in cards:
            if card1 == card2:
                continue

            c1s = []
            for i, c in enumerate(hand):
                if c == card1 or c == "J":
                    c1s.append(i)

                if len(c1s) == 2:
                    break

            c2s = []
            for i, c in enumerate(hand):
                if i in c1s:
                    continue

                if c == card2 or c == "J":
                    c2s.append(i)

                if len(c2s) == 2:
                    break

            if len(c1s) == 2 and len(c2s) == 2:
                return 2

    for card in cards:
        if len([c for c in hand if c == card or c == "J"]) == 2:
            return 1

    return 0


def rank(hand: str) -> tuple[int, ...]:
    return (type(hand),) + tuple(cards.find(c) for c in hand)


def rank2(hand: str) -> tuple[int, ...]:
    return (type2(hand),) + tuple(cards2.find(c) for c in hand)


vs = []
for line in lines:
    hand, score = line.split()
    vs.append((hand, int(score)))


def cmp(a, b):
    ar = rank(a[0])
    br = rank(b[0])

    if ar < br:
        return -1
    elif ar == br:
        return 0
    else:
        return 1


def cmp2(a, b):
    ar = rank2(a[0])
    br = rank2(b[0])

    if ar < br:
        return -1
    elif ar == br:
        return 0
    else:
        return 1


puzzle.answer_a = sum(
    (i + 1) * v for i, (_, v) in enumerate(sorted(vs, key=cmp_to_key(cmp)))
)
puzzle.answer_b = sum(
    (i + 1) * v for i, (_, v) in enumerate(sorted(vs, key=cmp_to_key(cmp2)))
)
