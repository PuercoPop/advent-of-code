move_map = {
        '^': complex( 0,  1),
        'v': complex( 0, -1),
        '<': complex(-1,  0),
        '>': complex( 1,  0),
        }

# Part 1
with open('3.input', 'r') as f:
    path=[]
    santa_pos=complex(0, 0)
    path.append(santa_pos)
    for char in f.read():
        santa_pos += move_map[char]
        path.append(santa_pos)
    print(len(set(path)))

# Part 2
with open('3.input', 'r') as f:
    santa_path=[]
    robo_santa_path=[]
    santa_pos=complex(0, 0)
    robo_santa_pos=complex(0, 0)
    path.append(santa_pos)
    for i, char in enumerate(f.read()):
        if i % 2 == 0:
            santa_pos += move_map[char]
            santa_path.append(santa_pos)
        else:
            robo_santa_pos += move_map[char]
            robo_santa_path.append(robo_santa_pos)
    print(len(set(santa_path+robo_santa_path)))
