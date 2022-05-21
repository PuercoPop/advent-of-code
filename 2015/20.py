# Binary search doesn't work because the score is not monotonically increasing.
# $ time python3 20.py
# 776160

# real    274m13.351s
# user    274m6.338s
# sys     0m0.904s
from math import ceil
goal=33100000
n=1
def s(n): return sum([i*10 for i in range(1, n+1) if n % i == 0])
# def s2(n): return sum([i*11 for i in range(max(1, n-50+1), n+1) if n % i == 0])
while s(n)<(goal*0.5): n=ceil(1.1*n)
print("checkpoint: ", n)
while s(n)<goal: n+=1
print(n)
