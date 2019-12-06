import sys

for line in sys.stdin:
    if line.endswith("\n"):
        line = line[:-1]

    expr = line.split(",")
    expr_old = [int(x) for x in expr]

    for x in range(100):
        for y in range(100):

            expr = expr_old.copy()

            expr[1] = x
            expr[2] = y

            i = 0

            while True:
                op = expr[i]

                if op == 99:
                    if expr[0] == 19690720:
                        print(expr[1])
                        print(expr[2])
                    break

                pos1 = expr[i + 1]
                pos2 = expr[i + 2]
                store_pos = expr[i + 3]

                if op == 1:
                    expr[store_pos] = expr[pos1] + expr[pos2]
                elif op == 2:
                    expr[store_pos] = expr[pos1] * expr[pos2]

                i += 4
