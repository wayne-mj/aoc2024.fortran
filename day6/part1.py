def get_turtle_pos():
    for r, row in enumerate(lines):
        for c, val in enumerate(row):
            if val == "^":
                return (r,c)
            

with open("input1.txt", 'r') as f:
    lines = list(map(str.strip, f.readlines()))

num_rows = len(lines)
num_cols = len(lines[0])

r,c = get_turtle_pos()
dr,dc = -1,0
lookup = set()

while True:
    lookup.add((r, c))
    if not (0 <= r + dr < num_rows and 0 <= c + dc < num_cols):
        break
    if lines[r+dr][c+dc] == "#":
        dc,dr = -dr,dc
    else:
        r += dr
        c += dc


print(f"{len(lookup)}")