example = [20, 15, 10, 5, 5]
input=[]
with open('17.input', 'r') as f:
    for line in f.readlines():
        input.append(int(line))
input.sort()

def solve_1(input, target):
    combinations=set()
    q=[[i for i in range(0, len(input))]]
    while len(q)>0:
        current=q.pop()
        # print(current)
        total=sum([input[ix] for ix in current])
        if total > target:
            q.extend([current[0:i]+current[1+i:] for i in range(0, len(current))])
        elif total == target:
            sig=",".join([str(ix) for ix in current])
            combinations.add(sig)
    return len(combinations)

if __name__ == "__main__":
    # print(solve_1(example, 25))
    print(solve_1(input, 150))
