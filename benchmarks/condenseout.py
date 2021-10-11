# Condense the output from the backend so that it only prints new facts
import fileinput
import re

facts = {}

def lookup(rel):
    if rel not in facts:
        facts[rel] = 0
        return 0
    else:
        return facts[rel]

for line in fileinput.input():
    if ":" in line:
        left = line.split(":")[0]
        m = re.search("(\d+) total", line)
        num = int(m.group(1))
        if num > lookup(left):
            facts[left] = num
            print("{}: {}".format(left, num))

