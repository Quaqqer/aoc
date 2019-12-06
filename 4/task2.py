min_val = 152085
max_val = 670283

def is_correct(num):
    num = str(num)
    last_val = 0
    double = 0
    temp = 0

    for char in num:
        val = int(char)
        if val == last_val:
            temp += 1
        elif val > last_val:
            if temp == 1:
                double += 1
            temp = 0
            last_val = val
        else:
            return False
    if temp == 1:
        double += 1

    if double > 0:
        return True
    return False


num = 0
for i in range(min_val, max_val+1):
    if is_correct(i):
        num += 1
print(num)
