import re
import random
import unittest
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

def initial_mix(datasheet, target):
    m=dict()
    for k in datasheet:
        m[k]=0
    m[random.choice(datasheet.keys())]=target
    return m

def mix_sig(m):
    s=''
    for k,v in m.items():
        s+=k
        s+=str(v)
    return s

def mix_score(mix, datasheet):
    props=['flavor', 'capacity', 'texture', 'durability']
    score=1
    for prop in props:
        sum=0
        for ingredient, quantity in mix.items():
            sum+=datasheet[ingredient][prop]*quantity
        score*=max(0, sum)
    return score

def find_neighbors(mix, upper_bound):
    can_remove=[k for k,v in mix.items() if v>0]
    can_add   =[k for k,v in mix.items() if v<upper_bound]
    changes=[(x,y) for x in can_remove for y in can_add]
    neighbors=list()
    for dec,inc in changes:
        n = mix.copy()
        n[dec]-=1
        n[inc]+=1
        neighbors.append(n)
    return neighbors


def solve_1(input, target):
    datasheet = read_datasheet(input)
    seed=initial_mix(datasheet, target)
    seen=dict()
    seen[mix_sig(seed)]=mix_score(seed,datasheet)
    q=[seed]
    while len(q)>0:
        next = q.pop()
        neighbors = find_neighbors(next, target)
        for neighbor in neighbors:
            if mix_sig(neighbor) not in seen.keys():
                q.append(neighbor)
                seen[mix_sig(neighbor)]=mix_score(neighbor,datasheet)
    return max(seen.values())

class Test(unittest.TestCase):
    def test_mix_sig(self):
        d= {'Butterscotch': 40,'Cinnamon': 60}
        self.assertEqual('Butterscotch40Cinnamon60', mix_sig(d))

if __name__ == "__main__":
    # unittest.main()
    # print(solve_1("15.example", 100))
    print(solve_1("15.input", 100))
