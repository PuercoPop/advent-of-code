import re

def combination(string):
    return re.match('ab|cd|pq|xq',string)

def doubles(string):
    found=False
    last_char=''
    for char in string:
        if char == last_char:
            found=True
            break
        last_char=char
    return found

def count_vowels(string):
    vowel_count = sum([ 1 for char in string if char in 'aeiou'])
    return vowel_count >= 3
