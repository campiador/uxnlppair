import os

def nonblank_lines(f):
    for l in f:
        line = l.rstrip()
        if line:
            yield line

count = 0

with open("1500reviews.txt") as f_in:
    for line in nonblank_lines(f_in):
        count += 1


print("There are: [" + str(count) + "] lines" )

