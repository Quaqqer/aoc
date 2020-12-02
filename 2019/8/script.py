import sys

dim = 25, 6
size = dim[0] * dim[1]


def get_zeros(layer):
    return layer.count("0")


def get_pixel(i, stri):
    ans = stri[i]
    if ans == "2":
        return get_pixel(i, stri[size:])
    return "#" if ans == "1" else " "


def print_image(stri):
    for y in range(dim[1]):
        str = ""
        for x in range(dim[0]):
            str += get_pixel(y * dim[0] + x, stri)
        print(len(str), str)


def calc(str):
    layers = int(len(str) / size)

    amt_zeros_min = -1
    lay = -1
    for i in range(layers):
        amt_zeros = get_zeros(str[i * size : (i + 1) * size])
        if amt_zeros_min == -1 or amt_zeros < amt_zeros_min:
            lay = i
            amt_zeros_min = amt_zeros

    layer = str[lay * size : (lay + 1) * size]
    print(layer.count("1") * layer.count("2"))

    print(lay, amt_zeros_min)


for line in sys.stdin:
    line = line[:-1] if line.endswith("\n") else line

    res = calc(line)
    print_image(line)
