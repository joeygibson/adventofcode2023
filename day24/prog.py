#!/usr/bin/env python3
import itertools
import re
import sys
import unittest

import sympy


class HailStone:
    def __init__(self, x: float, y: float, z: float, vx: float, vy: float, vz: float):
        self.x = x
        self.y = y
        self.z = z
        self.vx = vx
        self.vy = vy
        self.vz = vz

    def __repr__(self):
        return f'HailStone({self.x}, {self.y}, {self.z}, {self.vx}, {self.vy}, {self.vz})'

    def break_up(self) -> tuple[float, float, float, float, float, float]:
        return self.x, self.y, self.z, self.vx, self.vy, self.vz

    def one_nano_later(self) -> tuple[float, float, float]:
        return (self.x + self.vx,
                self.y + self.vy,
                self.z + self.vz)

    def line(self) -> tuple[float, float, float]:
        (x2, y2, z2) = self.one_nano_later()

        a = self.y - y2
        b = x2 - self.x
        c = (self.x * y2) - (x2 * self.y)

        return a, b, -c


def parse(lines: list[str]) -> list[HailStone]:
    stones = []

    for line in lines:
        parts = line.split(' @ ')
        x, y, z = parts[0].split(',')
        vx, vy, vz = parts[1].split(',')

        stones.append(HailStone(float(x), float(y), float(z), float(vx), float(vy), float(vz)))

    return stones


def find_intersection(stone1: HailStone, stone2: HailStone,
                      lower_bound: float, upper_bound: float) -> tuple[float, float]:
    l1 = stone1.line()
    l2 = stone2.line()

    d = (l1[0] * l2[1]) - (l1[1] * l2[0])
    dx = (l1[2] * l2[1]) - (l1[1] * l2[2])
    dy = (l1[0] * l2[2]) - (l1[2] * l2[0])

    if d != 0:
        x = dx / d
        y = dy / d

        tx1 = (x - stone1.x) / stone1.vx
        tx2 = (x - stone2.x) / stone2.vx

        if tx1 >= 0 and tx2 >= 0 \
                and lower_bound <= x <= upper_bound \
                and lower_bound <= y <= upper_bound:
            return float(x), float(y)


def part1(lines: list[str], lower_bound: float, upper_bound: float) -> float:
    # from https://topaz.github.io/paste/#XQAAAQDXAQAAAAAAAAAzHIoib6p4r/McpYgEEgWhHoa5LSRMkVi92ASWXgRJn/53WGzZWzJlNzR/LeXSZEQnBkC+jD+efAupRol5bOfXbJwxvGQUitWOYhQnNhp1IIb+hfC8AaLOFmv4wIp5CzBQKrm28BEIBOYFbMPy6M2OXUGYq6JCT3QdyxTKD9DDQMSJxrkOB+NWjG5qDNsvSBbbPvS8lG4/FpPx+2veExcgoc1tAwTU3Qm0SgsCtVQWHI8I9jxt0YehRiYZefxqvZeYrbI8+6F96APmhePvuzdZx6sQKKM7WruVIJMd2iHAHtqgpWUDcatGq6vrkXen6cKjBzq8duXJkYDM8SV3HoxzxXUgJfO9HTRN5uEMWvpTuvENABmirX26SLG3RvsnrTclw2wWBwFbEjx1XcLavGTkik//6bcLRA==

    stones = [[int(i) for i in l.replace('@', ',').split(',')]
              for l in lines]
    hits = 0

    for pair in itertools.combinations(stones, 2):
        (x, y, _, dx, dy, _), (u, v, _, du, dv, _) = pair

        if dy * du == dx * dv:
            continue

        t1 = (dv * (x - u) - du * (y - v)) / (dy * du - dx * dv)
        t2 = (dy * (u - x) - dx * (v - y)) / (dv * dx - du * dy)

        if t1 > 0 and 2e14 < x + t1 * dx < 4e14 \
                and t2 > 0 and 2e14 < y + t1 * dy < 4e14:
            hits += 1

    return hits


def part2(lines: list[str]) -> float:
    # from https://github.com/marcodelmastro/AdventOfCode2023/blob/main/Day24.ipynb
    stones = []

    for line in lines:
        m = re.match(r'(\d+),\s+(\d+),\s+(\d+)\s+@\s+(-?\d+),\s+(-?\d+),\s+(-?\d+)', line)
        x = (int(m.group(1)), int(m.group(2)), int(m.group(3)))
        v = (int(m.group(4)), int(m.group(5)), int(m.group(6)))
        stones.append((x, v))

    x = sympy.symbols('x')
    y = sympy.symbols('y')
    z = sympy.symbols('z')
    vx = sympy.symbols('vx')
    vy = sympy.symbols('vy')
    vz = sympy.symbols('vz')

    (x1, y1, z1), (vx1, vy1, vz1) = stones[0]
    (x2, y2, z2), (vx2, vy2, vz2) = stones[1]
    (x3, y3, z3), (vx3, vy3, vz3) = stones[2]

    solutions = sympy.solve(
        [(x - x1) * (vy - vy1) - (y - y1) * (vx - vx1), (y - y1) * (vz - vz1) - (z - z1) * (vy - vy1),
         (x - x2) * (vy - vy2) - (y - y2) * (vx - vx2), (y - y2) * (vz - vz2) - (z - z2) * (vy - vy2),
         (x - x3) * (vy - vy3) - (y - y3) * (vx - vx3), (y - y3) * (vz - vz3) - (z - z3) * (vy - vy3)],
        [x, y, z, vx, vy, vz], dict=True)

    for s in solutions:
        if s[vx] == int(s[vx]) and s[vy] == int(s[vy]) and s[vz] == int(s[vz]):
            print(s)
            break

    return s[x] + s[y] + s[z]


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines, 7, 27)
        self.assertEqual(2, res)

    def test_part1_2(self):
        with open('input1.txt') as f:
            self.lines = f.read().splitlines()

        res = part1(self.lines, 200_000_000_000_000, 400_000_000_000_000)
        self.assertEqual(12783, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    # print(f'part1 -> {part1(lines, 7, 27)}')
    print(f'part1 -> {part1(lines, 200_000_000_000_000, 400_000_000_000_000)}')
    print(f'part2 -> {part2(lines)}')
