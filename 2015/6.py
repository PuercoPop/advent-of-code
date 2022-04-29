import re

grid=[ [ False for n in range(1000) ] for n in range(1000) ]
operations =  {
        "on": lambda cell: True,
        "off": lambda cell: False,
        "toggle": lambda cell: not cell,
}

def execute(grid, instruction):
    op, from_x, from_y, to_x, to_y = instruction
    for x in range(from_x, to_x+1):
        for y in range(from_y, to_y+1):
            grid[x][y] = operations[op](grid[x][y])
    return grid

def parse_line(line):
    m=re.match(r".*?(\w+) (\d+),(\d+) through (\d+),(\d+)", line)
    return [m.group(1), int(m.group(2)), int(m.group(3)), int(m.group(4)), int(m.group(5))]

with open('6.input', 'r') as f:
    instructions = [parse_line(line) for line in f.readlines()]

for inst in instructions:
    grid=execute(grid, inst)
day_1=sum([row.count(True) for row in grid])
print(day_1)
