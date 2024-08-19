from collections import Counter

from aocd.models import Puzzle

from util import print_grid

puzzle = Puzzle(2019, int("08"))
data = list(map(int, puzzle.input_data))

w = 25
h = 6
pixels = w * h

n_layers = len(data) // (w * h)
layers = [data[pixels * i : pixels * (i + 1)] for i in range(n_layers)]

vs = []
for i, layer in enumerate(layers):
    c = Counter(layer)
    vs.append((c[0], c[1] * c[2]))
puzzle.answer_a = min(vs)[1]

pic: list[list[str]] = [["?" for _ in range(w)] for _ in range(h)]

for layer in layers:
    for x in range(w):
        for y in range(h):
            px = layer[x + w * y]
            if pic[y][x] == "?" and px != 2:
                if px == 0:
                    pic[y][x] = " "
                else:
                    pic[y][x] = "█"

print_grid(pic)

print("What 5 letters do you see?")
ans = input("> ")
assert len(ans) == 5
puzzle.answer_b = ans
