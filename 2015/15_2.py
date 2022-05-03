import re
from itertools import combinations_with_replacement

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

def compress(mix):
    h=dict()
    for ingredient in mix:
        h[ingredient] = h.get(ingredient, 0) + 1
    return h

def mix_score(mix, datasheet):
    props=['flavor', 'capacity', 'texture', 'durability']
    hist=compress(mix)
    score=1
    for prop in props:
        sum=0
        for ingredient, quantity in hist.items():
            sum+=datasheet[ingredient][prop]*quantity
        score*=max(0, sum)
    return score

def mix_calories(mix, datasheet):
    return sum([datasheet[ingredient]['calories'] for ingredient in mix])

def solve_1(input, target):
    datasheet = read_datasheet(input)
    mixes=combinations_with_replacement(datasheet.keys(), target)
    mixes=[m for m in mixes if mix_calories(m, datasheet)== 500]
    max_score=max([mix_score(m, datasheet) for m in mixes])
    return max_score

if __name__ == "__main__":
    # print(solve_1("15.example", 100))
    print(solve_1("15.input", 100))
