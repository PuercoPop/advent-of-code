# -*- coding: utf-8 -*-

from kanren import facts, Relation, run, var, conde, lall, lany, eq
from kanren.core import success

edges = Relation()
right = Relation()
down = Relation()
left = Relation()

facts(edges,
      (1, 'U', 1),
      (2, 'U', 2),
      (3, 'U', 3),
      (4, 'U', 1),
      (5, 'U', 2),
      (6, 'U', 3),
      (7, 'U', 4),
      (8, 'U', 5),
      (9, 'U', 6),

      (1, 'D', 4),
      (2, 'D', 5),
      (3, 'D', 6),
      (4, 'D', 7),
      (5, 'D', 8),
      (6, 'D', 9),
      (7, 'D', 7),
      (8, 'D', 8),
      (9, 'D', 9),

      (1,'R', 2),
      (2,'R', 3),
      (3,'R', 3),
      (4,'R', 5),
      (5,'R', 6),
      (6,'R', 6),
      (7,'R', 8),
      (8,'R', 9),
      (9,'R', 9),

      (1,'L', 1),
      (2,'L', 1),
      (3,'L', 2),
      (4,'L', 4),
      (5,'L', 4),
      (6,'L', 5),
      (7,'L', 7),
      (8,'L', 7),
      (9,'L', 8))

# def walk(pos, path, end_pos):
#     x = var()
#     lany(lall(eq(path, []), eq(pos, end_pos)),
#          lall(
#               edges(pos, path[0], x),
#               walk(x, path[1:], end_pos)))

def walk(pos, path, end_pos):
    if eq(path, []):
        eq(pos, end_pos)
        return success
    else:
        x = var()
        return conde(edges(pos, path[0], x),
                     walk(x, path[1:], end_pos))

x = var('x')
y = var('y')
print(run(1, y, walk(5, ['L', 'L', 'U'], x),
          walk(y,[],x)
      )) # 3
