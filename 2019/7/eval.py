import itertools

line = "3,8,1001,8,10,8,105,1,0,0,21,34,43,64,85,98,179,260,341,422,99999,3,9,1001,9,3,9,102,3,9,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,2,9,1002,9,4,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,102,3,9,9,101,4,9,9,102,3,9,9,4,9,99,3,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99"


class IntComputer:

    INCREMENTS = {
        99: 1,
        1: 4,
        2: 4,
        3: 2,
        4: 2,
        5: 3,
        6: 3,
        7: 4,
        8: 4,
        9: 2,
    }

    def __init__(self, string):
        self.expr = [int(i) for i in string.split(",")] + [0 for i in range(10000)]
        self.i = 0
        self.toggle = False

    def get_opstr(self):
        opstr = str(self.expr[self.i])

        # Make opstr 012 to 00012 etc.
        for j in range(5 - len(opstr)):
            opstr = "0" + opstr
        return opstr

    def get_pos(self, val):
        opstr = self.get_opstr()
        mode = int(opstr[3 - val])
        if mode == 0:
            return self.expr[self.i + val]
        elif mode == 1:
            return self.i + val

    def get_arg(self, val):
        opstr = self.get_opstr()
        mode = int(opstr[2 - val])
        pos = self.get_pos(val)
        return self.expr[pos]

    def run(self):
        while True:
            should_inc = True

            op = int(self.get_opstr()[-2:])

            # Exit
            if op == 99:
                print(f"Exit code: {self.expr[0]}")
                return None

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

                if self.toggle:
                    val = int(self.outpt)
                else:
                    val = int(self.phase)
                self.toggle = not self.toggle

                self.expr[store_pos] = val

            # Output
            elif op == 4:
                arg1 = self.get_arg(1)
                self.i += self.INCREMENTS[op]
                return arg1

            # Jump self.if false
            elif op == 5:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                if arg1 != 0:
                    should_inc = False
                    self.i = arg2

            # Jump self.if true
            elif op == 6:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                if arg1 == 0:
                    should_inc = False
                    self.i = arg2

            # Set true self.if less than
            elif op == 7:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                store_pos = self.get_pos(3)
                if arg1 < arg2:
                    self.expr[store_pos] = 1
                else:
                    self.expr[store_pos] = 0

            # Set true self.if equals
            elif op == 8:
                arg1 = self.get_arg(1)
                arg2 = self.get_arg(2)
                store_pos = self.get_pos(3)
                if arg1 == arg2:
                    self.expr[store_pos] = 1
                else:
                    self.expr[store_pos] = 0

            if should_inc:
                self.i += self.INCREMENTS[op]


mx = -1
perm = []

for i in itertools.permutations(["0", "1", "2", "3", "4"]):
    ipcs = []
    for k in range(5):
        ipc = IntComputer(line)
        ipcs.append(ipc)

    outpt = "0"
    for j in range(5):
        ipcs[j].outpt = outpt
        ipcs[j].phase = i[j]
        outpt = ipcs[j].run()

    if mx == -1 or outpt > mx:
        mx = outpt
        perm = i

print("Answer 1:", mx)

mx = -1
perm = []
for i in itertools.permutations(["5", "6", "7", "8", "9"]):
    ipcs = []
    for k in range(5):
        ipc = IntComputer(line)
        ipcs.append(ipc)

    outpt = "0"
    j = 0
    running = True
    toggle = False
    while running:
        ipcs[j].toggle = toggle
        if not toggle:
            ipcs[j].phase = i[j]
        ipcs[j].outpt = outpt
        temp = ipcs[j].run()
        if temp == None:
            running = False
        else:
            outpt = temp
        j = j + 1
        if j == 5:
            toggle = True
            j = 0

    if mx == -1 or outpt > mx:
        mx = outpt
        perm = i

print("Answer 2:", mx)
