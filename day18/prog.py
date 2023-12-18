#!/usr/bin/env python3

import sys
import unittest


def parse(lines: list[list[str]]) -> list[tuple[str, int, str]]:
    plan = []

    for line in lines:
        chunks = line.split(' ')
        plan.append((chunks[0], int(chunks[1]), chunks[2]))

    return plan


def dig(plan: list[tuple[str, int, str]]) -> list[tuple[int, int]]:
    pos = (0, 0)
    trench = [pos]

    for (dir, units, _) in plan:
        for t in range(units):
            if dir == 'U':
                pos = (pos[0], pos[1] - 1)
            elif dir == 'D':
                pos = (pos[0], pos[1] + 1)
            elif dir == 'R':
                pos = (pos[0] + 1, pos[1])
            elif dir == 'L':
                pos = (pos[0] - 1, pos[1])

            if pos == (0, 0):
                break

            trench.append(pos)

    return trench


def shoelace_area(trench):
    S1 = 0
    S2 = 0
    for i, (x1, y1) in enumerate(trench):
        if i + 1 == len(trench):
            x2, y2 = trench[0]
        else:
            x2, y2 = trench[i + 1]

        S1 += x1 * y2
        S2 += x2 * y1
    area = int(abs(S1 - S2) / 2)
    return area


def part1(lines: list[list[str]]) -> int:
    plan = parse(lines)

    trench = dig(plan)

    area = shoelace_area(trench)

    return len(trench) // 2 + area + 1


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(62, res)

    def test_part2(self):
        res = part2(self.lines)
        self.assertEqual(952408144115, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
