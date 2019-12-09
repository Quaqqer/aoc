import sys

ans = 0


def calc_fuel(num):
    return (num // 3) - 2


for line in sys.stdin:
    if line.endswith("\n"):
        line = line[:-1]

    num = int(line)

    dans = 0

    temp = calc_fuel(num)

    while True:
        if temp > 0:
            dans += temp
            print(temp)
        else:
            break
        temp = calc_fuel(temp)

    ans += dans

print(ans)
