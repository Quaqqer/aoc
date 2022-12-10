# 10   00:20:23  4094      0   00:42:25  4125      0
# i sleep

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("10"))
id = puzzle.input_data

# Main code
cycles = 1
x = 1
ans_a = 0
pixels = [[] for _ in range(7)]
# The first line gets a bit wonky, works good enough
pixels[0].extend([" ", " "])


def maybe_add_score():
    global ans_a
    if (cycles - 20) % 40 == 0:
        ans_a += x * cycles


def draw_pixel():
    sprite = ["#" if abs(i - x) <= 1 else " " for i in range(40)]

    row = cycles // 40
    col = cycles % 40

    pixels[row].append(sprite[col - 1])


for i, line in enumerate(id.splitlines()):
    words = line.split()

    match words[0]:
        case "noop":
            cycles += 1
        case "addx":
            cycles += 1
            draw_pixel()
            maybe_add_score()
            cycles += 1
            x += int(words[1])

    maybe_add_score()
    draw_pixel()


puzzle.answer_a = ans_a

for row in pixels:
    print("".join(row))
