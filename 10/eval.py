import sys
import math

asteroids = {}

class Asteroid:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.los = {}
        self.destroyed = False

    def __repr__(self):
        return f"Asteroid ({self.x}, {self.y})"

    def calc_los(self, asteroids):
        for asteroid in asteroids.values():
            if asteroid == self:
                continue

            dx, dy = asteroid.x - self.x, asteroid.y - self.y
            angle = math.atan2(dy, dx)
            
            if angle in self.los:
                insert(self, asteroid, self.los[angle])
            else:
                self.los[angle] = [asteroid]

    def loss(self):
        return len(self.los)

    def destroy(self):
        self.destroyed = True

def insert(src, ast, lst):
    dist1 = math.hypot(src.x - ast.x, src.y - ast.y)
    for i in range(len(lst)):
        ast2 = lst[i]
        dist2 = math.hypot(src.x - ast2.x, src.y - ast2.y)
        if dist1 < dist2:
            lst.insert(0, ast)
            return
    lst.append(ast)


y = 0
for line in sys.stdin:
    if line.endswith("\n"):
        line = line[:-1]

    for x in range(len(line)):
        if line[x] == "#":
            asteroid = Asteroid(x, y)
            asteroids[(x, y)] = asteroid
    
    y += 1

x, y = None, None
mx = -1
best = None
for asteroid in asteroids.values():
    asteroid.calc_los(asteroids)
    if mx == -1 or asteroid.loss() > mx:
        mx = asteroid.loss()
        x, y = asteroid.x, asteroid.y
        best = asteroid

print("Answer 1: " + str(mx))

def sort_los(item):
    return (item[0] + 0.5 * math.pi) % (2 * math.pi)

killed = 0
slos = list(best.los.items())
slos.sort(key=sort_los)
old_ang = -123
for los in slos:
    print(los[0])

running = True
i = -1
while slos:
    i = (i + 1) % len(slos)
    ang, asts = slos[i]
    if not asts:
        continue
    old_ang = ang
    ast = asts.pop(0)
    ast.destroy()
    killed += 1
    # print(killed, ast.x, ast.y)

    if killed == 200:
        ans = "Answer 2: " + str(ast.x * 100 + ast.y)
    if killed == len(asteroids) - 1:
        break
print(ans)
