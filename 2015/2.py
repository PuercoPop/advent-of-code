with open('2.input', 'r') as f:
    for line in f.readlines():
        [L, W, H] = [int(x) for x in line.split('x')]
        area=... 

# extracting area into a function 
def area(line):
    [L, W, H] = [int(x) for x in line.split('x')]
    return 2*L*W + 2*W*H + 2*H*L + min(L*W, W*H, H*L)

with open('2.input', 'r') as f:
    print(sum([ area(line) for line in f.readlines() ]))
