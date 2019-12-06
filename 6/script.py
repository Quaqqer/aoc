import sys

orbits = {}
planets = set()

for line in sys.stdin:
    line = line[:-1]
    p1, p2 = tuple(line.split(")"))

    orbits[p2] = p1
    planets.add(p1)
    planets.add(p2)


def get_orbits(orbit):
    if orbit != "COM":
        return get_orbits(orbits[orbit]) + 1
    else:
        return 0


sum = 0
for planet in planets:
    sum += get_orbits(planet)
print(f"Amount of direct and indirect orbits: {sum}")

you_path = ["YOU"]
while you_path[0] != "COM":
    you_path.insert(0, orbits[you_path[0]])

san_path = ["SAN"]
while san_path[0] != "COM":
    san_path.insert(0, orbits[san_path[0]])

i = 0
while you_path[i] == san_path[i]:
    i += 1

ans = len(you_path) + len(san_path) - i * 2 - 2
print(f"Transfers necessary: {ans}")
