import re

total = 0
with open("input1", "r") as file:
    contents = file.read()

#pattern = r"mul\((\d+),(\d+)\)"
pattern = r"mul\((\d+),(\d+)\)"

matches = re.findall(pattern, contents)

for y in matches:
    num1, num2 = y
    total = total + (int(num1) * int(num2))
    print(f"{total}")