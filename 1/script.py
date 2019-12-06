import sys

ans = 0

for line in sys.stdin:
    if line.endswith("\n"):
        line = line[:-1]

    num = int(line)
    num = num // 3
    num -= 2

    ans += num

print(ans)
