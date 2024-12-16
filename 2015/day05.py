from aocd.models import Puzzle

puzzle = Puzzle(2015, int("05"))
data = puzzle.input_data
lines = data.splitlines()

for line in lines:
    pass

vowels = "aeiou"


def is_nice(s: str):
    three_vowels = len([c for c in s if c in vowels]) >= 3
    pair = any(a == b for a, b in zip(s, s[1:]))
    not_contains = not any(ss in s for ss in ["ab", "cd", "pq", "xy"])
    return three_vowels and pair and not_contains


def is_nice2(s: str):
    pairs = False
    for i in range(len(s) - 1):
        for j in range(i + 2, len(s) - 1):
            if s[i : i + 2] == s[j : j + 2]:
                pairs = True
    trip = any(a == c for a, c in zip(s, s[2:]))
    return pairs and trip


puzzle.answer_a = len([line for line in lines if is_nice(line)])
puzzle.answer_b = len([line for line in lines if is_nice2(line)])
