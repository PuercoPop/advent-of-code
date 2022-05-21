def read_rules(path):
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

if __name__ == "__main__":
    # print(read_rules("19.example"))
    # print(solve_1("19.example"))
    print(solve_1("19.example2"))
    # print(solve_1("19.input"))
