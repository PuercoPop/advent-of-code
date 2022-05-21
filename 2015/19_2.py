# Compress the longest sequence possible until we are down to the first
# character of the target sequence, ej. H in HF
def read_rules(path):
    # We want the rules to be reverse and ordered by length
    lines=[l.strip() for l in open(path, 'r')]
    rules=[]
    goals=[]
    for l in lines[:-2]:
        lhs,rhs=l.split("=>")
        if lhs.strip() == "e":
            goals.append(rhs.strip())
        else:
            rules.append([rhs.strip(), lhs.strip()])
    sorted(rules, key=lambda x:len(x[1]))
    molecule=lines[-1].strip()
    return [dict(rules), goals, molecule]

def solve_2(path):
    [rules, goals, initial_molecule]=read_rules(path)
    steps=0
    molecule=initial_molecule
    while steps < 1:
        for pattern, replacement in rules.items():
            if pattern == molecule[:len(pattern)]:
                molecule = replacement + molecule[len(pattern):]

        steps+=1

if __name__ == "__main__":
    # print(read_rules("19.example"))
    # print(read_rules("19.input"))
    print(solve_2("19.input"))
    # print(solve_2("19.example"))
    # print(solve_2("19.example3"))
    # print(solve_2("19.input"))
