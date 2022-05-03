import re
import random
from itertools import product

def read_datasheet(input):
    datasheet=dict()
    with open(input, 'r') as f:
        for line in f.readlines():
            result = re.match("^(\w+): (.*?)$", line)
            name = result.group(1)
            props = dict()
            for prop in result.group(2).split(","):
                [prop_name, prop_value] = prop.strip().split(" ")
                props[prop_name] = int(prop_value)
            datasheet[name]=props
    return datasheet

def mix_score(mix, datasheet):
    props=['flavor', 'capacity', 'texture', 'durability']
    score=1
    for prop in props:
        sum=0
        for ingredient, quantity in mix.items():
            sum+=datasheet[ingredient][prop]*quantity
        score*=max(0, sum)
    return score

def empty_mix(datasheet):
    return dict([(k, 0) for k in datasheet])

def next_steps(mix, upper_bound):
    can_add=[k for k,v in mix.items() if v<upper_bound]
    neighbors=list()
    for inc in can_add:
        n = mix.copy()
        n[inc]+=1
        neighbors.append(n)
    return neighbors

def solve_1(input, target):
    datasheet = read_datasheet(input)
    mixes=[empty_mix(datasheet)]
    ingredient_count=0
    while ingredient_count<target:
        n=list()
        for mix in mixes:
            for m in next_steps(mix, target):
                n.append(m)
        mixes = n
        max_score = max([mix_score(m, datasheet) for m in mixes])
        mixes = [ m for m in mixes if mix_score(m, datasheet) == max_score ]
        ingredient_count+=1
    return max_score# , mixes, [mix_score(m, datasheet) for m in mixes]

if __name__ == "__main__":
    # print(solve_1("15.example", 100))
    print(solve_1("15.input", 100))
