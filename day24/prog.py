#!/usr/bin/env python3
import itertools
import sys
import unittest


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
    stones = parse(lines)

    combos = itertools.combinations(stones, 2)

    intersections = {(s1, s2): find_intersection(s1, s2, lower_bound, upper_bound) for s1, s2 in combos}

    return sum(1 for i in intersections.values() if i)


def part2(lines: list[str]) -> float:
    pass


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
