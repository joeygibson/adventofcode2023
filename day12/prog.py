#!/usr/bin/env python3
import functools
import sys
import unittest


def parse(lines: list[str]) -> list[tuple[str, list[int]]]:
    data = []

    for line in lines:
        chunks = line.split()
        broken_spring_counts = [int(x) for x in chunks[1].split(',')]
        data.append((chunks[0], tuple(broken_spring_counts)))

    return data


@functools.cache
def calc(record, groups):
    # did we run out of groups? maybe still valid
    if not groups:
        # make sure no more damaged springs
        if '#' not in record:
            # This will return true even if record is empty, which is valid
            return 1
        else:
            # more damaged springs that aren't in groups
            return 0

    # more groups, but no more record
    if not record:
        # we can't fit, so exit
        return 0

    # look at the next element in each record and group
    next_character = record[0]
    next_group = groups[0]

    # logic that treats first character as `#`
    def pound():
        # if first is a `#`, then the first n characters must be
        # able to be treated as `#`, where n is the first group number
        this_group = record[:next_group]
        this_group = this_group.replace('?', '#')

        # if the next group can't fit all the damaged springs, then
        # this is an invalid path
        if this_group != next_group * '#':
            return 0

        # if the rest of the record is just the last group, then
        # we're done and there's only one possibility
        if len(record) == next_group:
            # make sure this is the last group
            if len(groups) == 1:
                return 1
            else:
                # there are more groups, so this is invalid
                return 0

        # ensure the character after the group is a `.`
        if record[next_group] in '?.':
            # it can be a separator, so skip it and reduce to the
            # next group
            return calc(record[next_group + 1:], groups[1:])

        # can't be handled, so exit
        return 0

    # logic that treats first characger as `.`
    def dot():
        # skip over dot, looking for next `#`
        return calc(record[1:], groups)

    if next_character == '#':
        # test pound logic
        out = pound()
    elif next_character == '.':
        # test dot logic
        out = dot()
    elif next_character == '?':
        # this could be either pound or dot, so explore both
        out = dot() + pound()
    else:
        raise Exception(f'Unknown character: {next_character}')

    # print(record, groups, ' -> ', out)

    return out


def part1(lines: list[str]) -> int:
    data = parse(lines)

    output = 0

    for record in data:
        output += calc(record[0], record[1])

    return output


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)

        self.assertEqual(21, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
