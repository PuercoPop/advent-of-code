import hashlib

input="bgvyzdsv"

def is_valid(candidate):
    h=hashlib.new('md5')
    h.update('{}{}'.format(input, candidate))
    return h.hexdigest()[0:6]=="000000"

for i in range(9999999):
    if is_valid(i):
        print(i)
        break
