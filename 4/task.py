min_pass = 152085
max_pass = 670283


def is_correct(num):
    last = 0
    doubles = 0
    largest_doubles = 0
    doubles_in_num = 0
    for char in num:
        n = int(char)
        if n > last:
            last = int(char)
            if doubles_in_num == 1:
                largest_doubles += 1
            doubles_in_num = 0
        elif n == last:
            doubles_in_num += 1
        else:
            return False
    if largest_doubles < 1:
        return False
    return True


num = 0
for i in range(min_pass, max_pass + 1):
    if is_correct(str(i)):
        num += 1
print(num)

print(is_correct("11222333"))
