import re
from copy import deepcopy
from collections import defaultdict
from itertools import permutations

floors = [[], [], [], []]
elevator_position = 0

regex = r"a (.*?) (generator|microchip)"

object_counter = 0

with open('11.input', 'r') as input_file:
    for floor_index, line in enumerate(input_file, start=0):
        if 'nothing relevant' in line:
            pass
        else:
            matches = re.finditer(regex, line)
            for match in matches:
                object_counter += 1
                material, thing_type = match.groups()
                if thing_type == 'microchip':
                    material = material.split('-')[0]
                floors[floor_index].append([material, thing_type])

# print floors

# find 2 items safe to bring upstairs
# find 1 item safe to bring upstairs
# find 1 item safe to bring downstairs
# find 2 items safe to bring downstairs
# if you can't, backtrack, avoiding loops (yay for only four floors)


def safe_floor(floor):
    if not floor:
        return True

    stuff = defaultdict(list)
    for item in floor:
        stuff[item[0]].append(item[1])

    return len(set(map(lambda x: 'microchip' in x, [group for index, group in stuff.iteritems() if len(group) == 1]))) < 2


def same_floors(f1, f2):
    return [sorted(floor) for floor in f1] == [sorted(floor) for floor in f2]


def process_floor(position, func_floors, previous_floors, level=0):
    # print func_floors[position]
    if position == 3 and len(func_floors[position]) == object_counter:
        # print 'Done'
        print level
        # print func_floors
        return True
    # bring upstairs
    if position < 3:
        for perm in permutations(func_floors[position], 2):
            # print perm, 'being analyzed'
            copy_floors = deepcopy(func_floors)
            if safe_floor(func_floors[position + 1] + list(perm)):
                map(copy_floors[position].remove, perm)
                # print perm, 'removed'
                if safe_floor(copy_floors[position]):
                    copy_floors[position + 1] += list(perm)
                    if not same_floors(copy_floors, previous_floors):
                        # print perm, 'going up'
                        if process_floor(position + 1, copy_floors, func_floors, level + 1):
                            return True
        for perm in func_floors[position]:
            # print perm, 'being analyzed'
            copy_floors = deepcopy(func_floors)
            if safe_floor(func_floors[position + 1] + [list(perm)]):
                copy_floors[position].remove(perm)
                # print perm, 'removed'
                if safe_floor(copy_floors[position]):
                    copy_floors[position + 1] += [list(perm)]
                    if not same_floors(copy_floors, previous_floors):
                        # print perm, 'going up'
                        if process_floor(position + 1, copy_floors, func_floors, level + 1):
                            return True
    # bring downstairs
    if position >= 0:
        for perm in func_floors[position]:
            # print perm, 'being analyzed'
            copy_floors = deepcopy(func_floors)
            if safe_floor(func_floors[position - 1] + [list(perm)]):
                copy_floors[position].remove(perm)
                # print perm, 'removed'
                if safe_floor(copy_floors[position]):
                    copy_floors[position - 1] += [list(perm)]
                    if not same_floors(copy_floors, previous_floors):
                        # print perm, 'going down'
                        if process_floor(position - 1, copy_floors, func_floors, level + 1):
                            return True
        for perm in permutations(func_floors[position], 2):
            # print perm, 'being analyzed'
            copy_floors = deepcopy(func_floors)
            if safe_floor(func_floors[position - 1] + list(perm)):
                map(copy_floors[position].remove, perm)
                # print perm, 'removed'
                if safe_floor(copy_floors[position]):
                    copy_floors[position - 1] += list(perm)
                    if not same_floors(copy_floors, previous_floors):
                        # print perm, 'going down'
                        if process_floor(position - 1, copy_floors, func_floors, level + 1):
                            return True

    return False


process_floor(elevator_position, floors, floors)

floors[0] += [['elerium', 'generator'],
              ['elerium', 'microchip'],
              ['dilithium', 'generator'],
              ['dilithium', 'microchip']]

object_counter += 4

process_floor(elevator_position, floors, floors)
