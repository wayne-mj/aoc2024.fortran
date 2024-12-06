import sys
from collections import defaultdict

with open("input1.txt", 'r') as f:
    data = f.read()

rules,jobs = data.split('\n\n')

rules = [tuple(map(int, vals.split('|'))) for vals in rules.splitlines()]
jobs = [tuple(map(int, vals.split(','))) for vals in jobs.splitlines()]

rules_map = defaultdict(bool)
for x,y in rules:
    rules_map[(y,x)] = True

def check_job(job):
    for i in range(len(job)):
        for j in range(i+1, len(job)):
            if rules_map[(job[i],job[j])]:
                return 0
    return job[len(job) // 2]

part1 = 0
for job in jobs:
    part1 += check_job(job)

print (f"{part1}")
# print (f"Rules :{rules}")
# print (f"Jobs: {jobs}")