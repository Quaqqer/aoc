import sys

class IntComputer:

    INCREMENTS = {
        99: 1,
        1: 4,
        2: 4,
        3: 2,
        4: 2,
    }

    def __init__(self, string):
        self.expr = [int(i) for i in string.split(",")]
        self.i = 0

    def get_op(self):
        return self.expr[self.i]

    def get_pos(self, val):
        return self.expr[self.i + val]

    def get_arg(self, val):
        pos = self.get_pos(val)
        return self.expr[pos]

    def run(self):
        while True:
            op = self.get_op()

            # Exit
            if op == 99:
                return self.expr[0]

            # Addition
            elif op == 1:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                store_pos = self.get_pos(3)

                self.expr[store_pos] = arg1 + arg2

            # Multiplication
            elif op == 2:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                store_pos = self.get_pos(3)

                self.expr[store_pos] = arg1 * arg2

            # Input
            elif op == 3:
                store_pos = self.get_pos(1)
                val = int(input("Input: "))
                self.expr[store_pos] = val

            # Output
            elif op == 4:
                arg1 = self.get_arg(1)
                print(arg1)

            self.i += self.INCREMENTS[op]


for line in sys.stdin:
    line = line[:-1] if line.endswith("\n") else line
    ipc = IntComputer(line)
    ans = ipc.run()
    print("Answer 1: " + str(ans))

for noun in range(100):
    for verb in range(100):
        ipc = IntComputer(line)
        ipc.expr[1] = noun
        ipc.expr[2] = verb
        ans = ipc.run()
        if ans == 19690720:
            print("Answer 2: " + str(100 * noun + verb))
            break
