# 15   00:02:25   155      0   00:11:47   133      0
# We're moving up in the world. I felt fast today, still not a single point.
# People are way too fast!

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("15"))
data = puzzle.input_data


def hash(s: str):
    h = 0
    for c in s:
        h += ord(c)
        h *= 17
        h %= 256
    return h


def part2():
    boxes = [[] for _ in range(256)]
    for w in data.split(","):
        if "=" in w:
            s, v = w.split("=")
            v = int(v)
            h = hash(s)
            for i in range(len(boxes[h])):
                if boxes[h][i][0] == s:
                    boxes[h][i] = (s, v)
                    break
            else:
                boxes[h].append((s, v))
        else:
            s, _ = w.split("-")
            h = hash(s)
            boxes[h] = [box for box in boxes[h] if box[0] != s]

    ans_b = 0
    for i, box in enumerate(boxes):
        for j, slot in enumerate(box):
            ans_b += slot[1] * (j + 1) * (i + 1)
    return ans_b


puzzle.answer_a = sum(hash(w) for w in data.split(","))
puzzle.answer_b = part2()
