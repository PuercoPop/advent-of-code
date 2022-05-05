def read_rules(path):
    rules=[]
    molecule=''
    with open(path, 'r') as f:
        reading_rules=True
        for line in f.readlines():
            if line.strip() == "":
                reading_rules=False
                next
            if reading_rules:
                rules.append([x.strip() for x in line.split("=>")])
            else:
                molecule=line.strip()
    return [rules, molecule]

def solve_1(path):
    combinations=set()
    [rules, molecule]=read_rules(path)
    for i in range(0, len(molecule)):
        seen=molecule[0:i]
        looking_at=molecule[i:]
        for seq, rep in rules:
            if seq == looking_at[:len(seq)]:
                combinations.add("{}{}{}".format(seen, rep, looking_at[len(seq):]))
    return len(combinations)

def solve_2(path):
    [rules, target_molecule]=read_rules(path)
    seen=set()
    q=[[0, 'e']]
    while len(q)>0:
        step, molecule = q.pop()
        for i in range(0, len(molecule)):
            p=molecule[0:i]
            n=molecule[i:]
            for seq, rep in rules:
                if seq == n[:len(seq)]:
                    nm="{}{}{}".format(p, rep, n[len(seq):])
                    if nm == target_molecule:
                        return step+1
                    if nm not in seen and len(nm) <= len(target_molecule):
                        seen.add(nm)
                        q.append([step+1,nm])




if __name__ == "__main__":
    # print(solve_1("19.example"))
    # print(solve_1("19.example2"))
    # print(solve_1("19.input"))
    # print(solve_2("19.example"))
    # print(solve_2("19.example3"))
    print(solve_2("19.input"))
