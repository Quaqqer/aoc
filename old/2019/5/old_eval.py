line = "3,9,8,9,10,9,4,9,99,-1,8"
expr = [int(i) for i in line.split(",")]


def run():
    i = 0

    while True:
        opstr = str(expr[i])
        if len(opstr) < 5:
            for j in range(5 - len(opstr)):
                opstr = "0" + opstr
        op = int(opstr[-2:])

        pm1 = int(opstr[2])
        pm2 = int(opstr[1])
        pm3 = int(opstr[0])

        if op == 99:
            print("Exit", expr[0])
            break

        if op == 1:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            store = expr[i + 3]

            expr[store] = val1 + val2

            i += 4

        elif op == 2:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            store = expr[i + 3]

            expr[store] = val1 * val2

            i += 4

        elif op == 3:
            store = expr[i + 1]
            expr[store] = int(input("Input: "))
            i += 2

        elif op == 4:
            store = expr[i + 1]
            print(expr[store])
            i += 2

        elif op == 5:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            if val1 != 0:
                i = val2
            i += 3

        elif op == 6:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            if val1 == 0:
                i = val2
            i += 3

        elif op == 7:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            if val1 < val2:
                expr[expr[i + 3]] = 1
            else:
                expr[expr[i + 3]] = 0
            i += 4

        elif op == 8:
            val1 = expr[expr[i + 1]] if pm1 == 0 else expr[i + 1]
            val2 = expr[expr[i + 2]] if pm2 == 0 else expr[i + 2]
            if val1 == val2:
                expr[expr[i + 3]] = 1
            else:
                expr[expr[i + 3]] = 0
            i += 4
        else:
            print("Stuck")
            break
        print(expr)


run()
