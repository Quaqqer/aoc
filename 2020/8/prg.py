#!/bin/python3

from aocd.models import Puzzle

puzzle = Puzzle(year=2020, day=8);
input = puzzle.input_data.split("\n")

# silver
run = set()
acc = 0
i = 0
while (i < len(input)):
    op = input[i][:3]
    v = int(input[i][4:])

    if i in run:
        print(f"Silver: {acc}")
        break
    run.add(i)

    if op == "acc":
        acc += v
        i += 1
    elif op == "jmp":
        i += v
    elif op == "nop":
        i += 1


# gold
for j in range(len(input)):
    if input[j][:3] == "jmp":
        input[j] = "nop" + input[j][3:]

        run = set()
        acc = 0
        i = 0
        while (i < len(input)):
            op = input[i][:3]
            v = int(input[i][4:])

            if i in run:
                input[j] = "jmp" + input[j][3:]
                break
            run.add(i)

            if op == "acc":
                acc += v
                i += 1
            elif op == "jmp":
                i += v
            elif op == "nop":
                i += 1
        else:
            print(f"Gold: {acc}")
            break
