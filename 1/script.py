import sys


def calc_fuel(num):
    return (num // 3) - 2

def recursive_calc_fuel(num):
    new_fuel = calc_fuel(num)
    print(new_fuel)
    if new_fuel <= 0:
        return 0
    return new_fuel + recursive_calc_fuel(new_fuel)

ans1 = 0
ans2 = 0
for line in sys.stdin:
    num = int(line)
    ans1 += calc_fuel(num)
    ans2 += recursive_calc_fuel(num)

print("Answer 1: " + str(ans1))
print("Answer 2: " + str(ans2))
