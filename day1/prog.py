#!/usr/bin/env python3
import sys

with open(sys.argv[1]) as f:
    lines = f.read().splitlines()

sum = 0

for line in lines:
    digits = [c for c in line if c.isdigit()]

    num = int(f'{digits[0]}{digits[-1]}')

    sum += num

print(sum)

