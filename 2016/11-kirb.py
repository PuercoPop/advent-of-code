class State(object):

    def __init__(self, floor, layout, depth=0, previous=None):
        self.floor = floor
        self.layout = layout
        self.number_of_items = 12
        self.depth = depth
        self.previous = (previous or []) + [self]

    def __repr__(self):
        return pretty_print_layout(self.layout, self.floor)

    def get_unique_id(self):
        return frozenset(list(enumerate(self.layout)) + [self.floor])

    def get_floor(self, floor_index):
        shifted = self.layout >> (self.number_of_items * floor_index)
        return int('1' * self.number_of_items, 2) & shifted

    def build_layout(self, new_layout):
        return new_layout
        out = 0
        for index, values in enumerate(new_layout):
            out |= values<<(index*self.number_of_items)
        return out

    def get_neighbors(self):

        floors = self.layout
        single_item_changes = [(1<<index) for index in range(self.number_of_items)
                               if (1<<index) & floors[self.floor]]
        two_item_changes = []
        for index, item in enumerate(single_item_changes):
            for second_item in single_item_changes[index+1:]:
                two_item_changes.append(item | second_item)
        changes = single_item_changes + two_item_changes

        previous_floor = None if self.floor == 0 else floors[self.floor-1]
        current_floor = floors[self.floor]
        next_floor = None if self.floor == 3 else floors[self.floor+1]

        # validate changes
        valid_neighbors = []
        for possible_change in changes:
            updated_current_floor = current_floor & (~possible_change)
            if not floor_is_valid(updated_current_floor, self.number_of_items):
                continue

            # check if change can be applied to the next floor
            if (next_floor is not None
                    and floor_is_valid(next_floor | possible_change,
                                       self.number_of_items)):
                new_floors = floors[:]
                new_floors[self.floor] = updated_current_floor
                new_floors[self.floor + 1] |= possible_change
                new_layout = self.build_layout(new_floors)
                valid_neighbors.append(State(self.floor+1,
                                             new_layout,
                                             self.depth+1,
                                             self.previous))

            # check if change can be applied to the previous floor
            if (previous_floor is not None
                    and floor_is_valid(previous_floor | possible_change,
                                       self.number_of_items)):
                new_floors = floors[:]
                new_floors[self.floor] = updated_current_floor
                new_floors[self.floor - 1] |= possible_change
                new_layout = self.build_layout(new_floors)
                valid_neighbors.append(State(self.floor-1,
                                             new_layout,
                                             self.depth+1,
                                             self.previous))
        return valid_neighbors
