from aocd.models import Puzzle

puzzle = Puzzle(2015, int("16"))
data = puzzle.input_data
lines = data.splitlines()

sues: list[dict[str, int]] = []

for line in lines:
    sue = {}

    facts = line.split(": ", 1)[1].split(", ")
    for fact in facts:
        prop, n = fact.split(": ")
        n = int(n)
        sue[prop] = n

    sues.append(sue)

expected = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}

a_sues = list(range(len(sues)))
for fact, n in expected.items():
    a_sues = [sue for sue in a_sues if fact not in sues[sue] or sues[sue][fact] == n]

[a_sue] = a_sues
puzzle.answer_a = a_sue + 1


def b_cond(sue: dict[str, int], fact: str, n: int) -> bool:
    if fact not in sue:
        return True

    if fact in ("cats", "trees"):
        return sue[fact] > n
    elif fact in ("pomeranians", "goldfish"):
        return sue[fact] < n
    else:
        return sue[fact] == n


b_sues = list(range(len(sues)))
for fact, n in expected.items():
    b_sues = [sue for sue in b_sues if b_cond(sues[sue], fact, n)]
[b_sue] = b_sues
puzzle.answer_b = b_sue + 1
