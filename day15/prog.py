#!/usr/bin/env python3

import sys
import unittest
from dataclasses import dataclass


def hash(item: str) -> str:
    current_value = 0

    for val, ord_val in [(l, ord(l.strip())) for l in item]:
        current_value += ord_val
        current_value *= 17
        current_value = current_value % 256

    return current_value


def part1(data: str) -> int:
    chunks = data.split(',')

    total = 0
    for item in chunks:
        current_value = hash(item)

        total += current_value

    return total


@dataclass
class Lens:
    label: str
    focal_length: int


def part2(data: str) -> int:
    chunks = data.split(',')
    boxes = {}

    for instruction in chunks:
        if '=' in instruction:
            label, value = instruction.split('=')
            box_id = hash(label)
            lens = Lens(label, int(value))

            box = boxes.get(box_id, [])

            if any([l for l in box if l.label == label]):
                lens_index = next((i for i, l in enumerate(box) if l.label == label), -1)
                box[lens_index] = lens
            else:
                box.append(lens)
        else:
            label = instruction.strip('-')
            box_id = hash(label)
            box = boxes.get(box_id, [])

            if any([l for l in box if l.label == label]):
                lens_index = next((i for i, l in enumerate(box) if l.label == label), -1)
                box.pop(lens_index)

        boxes[box_id] = box

    total = 0

    for box_id, box in boxes.items():
        for pos, lens in enumerate(box):
            focus_power = (1 + int(box_id)) * (pos + 1) * lens.focal_length
            total += focus_power

    return total


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.data = f.read().replace('\n', '')

    def test_part0(self):
        res = part1('HASH')
        self.assertEqual(52, res)

    def test_part1(self):
        res = part1(self.data)
        self.assertEqual(1320, res)

    def test_part2(self):
        res = part2(self.data)
        self.assertEqual(145, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = f.read().replace('\n', '')

    print(f'part1 -> {part1(data)}')
    print(f'part2 -> {part2(data)}')
