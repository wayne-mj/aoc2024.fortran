import re
output = []

def process_stuff():
    total = 0
    with open("input1.txt", "r") as file:
        contents = file.read()

    mul_pattern = r"mul\((\d+),(\d+)\)"
    dont_pattern = r"don't\(\)"
    do_pattern = r"do\(\)"

    the_pattern = fr"({dont_pattern})|({do_pattern})|({mul_pattern})"
    matches = re.finditer(the_pattern, contents)

    ignore = False
    results = []

    for y in matches:
        if y.group(1):
            ignore = True
        elif y.group(2):
            ignore = False
        elif y.group(3):
            if not ignore:
                num1, num2 = int(y.group(4)), int(y.group(5))
                result = num1 * num2
                results.append(result)
    
    return results



output = process_stuff()

total = 0
for y in output:
    total = total + y

print (total)