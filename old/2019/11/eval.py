import time
line = "3,8,1005,8,324,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,29,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,50,1,1106,9,10,1,102,15,10,2,1003,3,10,1,3,19,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,89,1,1105,9,10,2,1103,1,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,119,1006,0,26,1,109,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,147,1006,0,75,1,1005,17,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,176,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,199,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,220,2,103,10,10,1,1,0,10,1,102,17,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,254,2,1001,10,10,1006,0,12,1,3,6,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,288,2,1106,9,10,2,1009,6,10,2,1101,18,10,2,103,8,10,101,1,9,9,1007,9,1045,10,1005,10,15,99,109,646,104,0,104,1,21101,838211318676,0,1,21102,341,1,0,1106,0,445,21101,0,838211051932,1,21101,0,352,0,1106,0,445,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,21704576195,1,21101,0,399,0,1106,0,445,21101,0,179356830951,1,21101,410,0,0,1105,1,445,3,10,104,0,104,0,3,10,104,0,104,0,21102,837897052948,1,1,21102,1,433,0,1106,0,445,21102,709052085092,1,1,21102,1,444,0,1105,1,445,99,109,2,21201,-1,0,1,21101,0,40,2,21102,476,1,3,21102,466,1,0,1105,1,509,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,471,472,487,4,0,1001,471,1,471,108,4,471,10,1006,10,503,1102,1,0,471,109,-2,2106,0,0,0,109,4,2102,1,-1,508,1207,-3,0,10,1006,10,526,21101,0,0,-3,21201,-3,0,1,21201,-2,0,2,21101,0,1,3,21101,545,0,0,1105,1,550,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,573,2207,-4,-2,10,1006,10,573,21201,-4,0,-4,1105,1,641,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,592,0,0,1105,1,550,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,611,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,633,21202,-1,1,1,21101,633,0,0,106,0,508,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0"


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
        self.rb = 0

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
        elif mode == 2:
            return self.expr[self.i + val] + self.rb

    def get_arg(self, val):
        opstr = self.get_opstr()
        mode = int(opstr[2 - val])
        pos = self.get_pos(val)
        return self.expr[pos]

    def run(self, inp):
        outputs = []
        while True:
            should_inc = True

            op = int(self.get_opstr()[-2:])

            # Exit
            if op == 99:
                print(f"Exit code: {self.expr[0]}")
                return

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
                val = inp
                self.expr[store_pos] = val

            # Output
            elif op == 4:
                arg1 = self.get_arg(1)
                outputs.append(arg1)
                if len(outputs) == 2:
                    self.i += self.INCREMENTS[op]
                    return outputs

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

            # Increment rb
            elif op == 9:
                arg1 = self.get_arg(1)
                self.rb += arg1
            else:
                print("Stuck")
                return

            if should_inc:
                self.i += self.INCREMENTS[op]

def turn_left(dx, dy):
    return -dy, dx

def turn_right(dx, dy):
    return dy, -dx

def paint(paints, px, py, dx, dy):
    maxx = -1000
    minx = 1000
    maxy = -1000
    miny = 1000
    for pos in paints:
        x, y = pos
        maxx = max(maxx, x)
        minx = min(x, minx)
        maxy = max(y, maxy)
        miny = min(y, miny)

    for y in range(maxy + 1, miny - 2, -1):
        string = ""
        for x in range(minx-1, maxx + 2):
            if (x, y) == (px, py):
                if (dx, dy) == (1, 0):
                    var = ">"
                elif (dx, dy) == (0, 1):
                    var = "^"
                elif (dx, dy) == (-1, 0):
                    var = "<"
                elif (dx, dy) == (0, -1):
                    var = "v"

                string += var
            elif (x, y) in paints:
                val = paints[(x, y)]
                var = " " if val == 0 else "#"
                string += var
            else:
                string += " "
        print(string)
    print("============================================")

ipc = IntComputer(line)
paints = {(0,0):1}
x, y = 0, 0
dx, dy = 0, 1
while True:

    if (x, y) in paints:
        inp = paints[(x, y)]
    else:
        inp = 0

    output = ipc.run(inp)
    if output and len(output) == 2:
        color = output[0]

        if output[1] == 0:
            dx, dy = turn_left(dx, dy)
        else:
            dx, dy = turn_right(dx, dy)

        paints[(x, y)] = color

        x += dx
        y += dy

    else:
        break

paint(paints, x, y, dx, dy)
print(f"Answer 1: {len(paints)}")
