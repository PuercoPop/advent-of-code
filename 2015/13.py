from itertools import permutations

def parse_line(line):
    xs = line.split(' ')
    src = xs[0]
    amount=int(xs[3])
    if xs[2] == 'lose':
        amount*=-1
    dst=xs[10][:-2]
    return [src, amount, dst]

def tally(seats, weights):
    total=0
    for i, p in enumerate(seats):
        l=(i-1)%len(seats)
        r=(i+1)%len(seats)
        left=seats[l]
        right=seats[r]
        total+=weights[p][left]
        total+=weights[p][right]
    return total


def solve_1(path):
    nodes=set([])
    weights=dict()
    with open(path, 'r') as f:
        for line in f:
            [src, amount, dst]=parse_line(line)
            nodes = nodes.union(set([src,dst]))
            if src not in weights.keys():
                weights[src] = dict()
            weights[src][dst]=amount
        max_score = 0
        for arrangement in permutations(nodes):
            score = tally(arrangement, weights)
            if score > max_score:
                max_score = score
        print(max_score)

def solve_2(path):
    nodes=set([])
    weights=dict()
    with open(path, 'r') as f:
        for line in f:
            [src, amount, dst]=parse_line(line)
            nodes = nodes.union(set([src,dst]))
            if src not in weights.keys():
                weights[src] = dict()
            weights[src][dst]=amount
        for k,v in weights.items():
            v['you']=0
        yd=dict()
        for n in nodes:
            yd[n]=0
        weights['you']=yd
        nodes=nodes.union(set(['you']))
        max_score = 0
        for arrangement in permutations(nodes):
            score = tally(arrangement, weights)
            if score > max_score:
                max_score = score
        print(max_score)


if __name__ == "__main__":
    # solve_1('./13.example')
    # solve_1('./13.input')
    solve_2('./13.input')
