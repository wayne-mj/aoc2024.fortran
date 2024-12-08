with open ("input1.txt", "r") as f:
    d = list(map(str.strip,f.readlines()))

def checkpath(left: int, right: list[int]) -> bool:
    if (len(right) ==1):
        return left == right[0]
    rightside = right.pop()
    if (left / rightside) == (left // rightside):
        if (checkpath(left // rightside, right[:])):
            return True
    if (left - rightside >= 0):
        if (checkpath(left-rightside, right[:])):
            return True
    return False

part1 = 0

for line in d:
    target = int(line.split(':')[0])
    elements = list(map(int, line.split(': ')[1].split(' ')))
    if (checkpath(target, elements)):
        part1 += target


print (f"{part1}")

