#!/usr/bin/env python3
import re
import sys
import unittest
from dataclasses import dataclass
from math import prod
from typing import Any


def parse_part(line: str) -> dict[str, int]:
    part = {}

    for item in line.replace('{', '').replace('}', '').split(','):
        var_name, value = item.split('=')
        part[var_name] = int(value)

    return part


def parse_input(data: str) -> tuple[list[str], list[str]]:
    rules = []
    parts = []

    sections = data.split('\n\n')

    for line in sections[0].splitlines():
        rules.append(line)

    for line in sections[1].splitlines():
        parts.append(parse_part(line))

    return rules, parts


@dataclass
class Step:
    var_name: str = None
    op: str = None
    value: int = 0
    dest: str = None


def parse_workflows(rules: list[str]) -> dict[str | Any, list[Step]]:
    workflows: dict[str | Any, list[Step]] = {}

    for rule in rules:
        match = re.match(r'(\w+){([^{]+)}', rule)

        steps = []

        name = match.group(1)
        raw_steps = match.group(2).split(',')

        for raw_step in raw_steps:
            if ':' in raw_step:
                rule, dest = raw_step.split(':')
                rule_match = re.match(r'(\w)([<>])(\d+)', rule)

                if rule_match:
                    var_name = rule_match.group(1)
                    op = rule_match.group(2)
                    value = int(rule_match.group(3))

                    step = Step(var_name, op, value, dest)
                else:
                    step = Step(dest=dest)
            else:
                step = Step(dest=raw_step)

            steps.append(step)

        workflows[name] = steps

    return workflows


def part1(lines: str) -> int:
    accepted: list[dict[str, int]] = []
    rejected: list[dict[str, int]] = []

    rules, parts = parse_input(lines)

    workflows = parse_workflows(rules)

    for part in parts:
        workflow = workflows['in']

        while workflow:
            for step in workflow:
                switch_to: str = None
                end: bool = False

                if step.var_name:
                    if step.op == '<':
                        if part[step.var_name] < step.value:
                            if step.dest == 'R':
                                rejected.append(part)
                                end = True
                            elif step.dest == 'A':
                                accepted.append(part)
                                end = True
                            else:
                                switch_to = step.dest
                        else:
                            continue  # evaluate the next step
                    elif step.op == '>':
                        if part[step.var_name] > step.value:
                            if step.dest == 'R':
                                rejected.append(part)
                                end = True
                            elif step.dest == 'A':
                                accepted.append(part)
                                end = True
                            else:
                                switch_to = step.dest
                        else:
                            continue
                elif step.dest == 'R':
                    rejected.append(part)
                    end = True
                elif step.dest == 'A':
                    accepted.append(part)
                    end = True
                else:
                    switch_to = step.dest

                if switch_to:
                    workflow = workflows[switch_to]
                    break
                elif end:
                    workflow = None
                    break

    return sum([sum(part.values()) for part in accepted])


@dataclass
class Span:
    min: int
    max: int

    def __len__(self):
        return self.max - self.min + 1

    def split(self, n: int) -> tuple['Span', 'Span']:
        if n < self.min or n >= self.max:
            raise ValueError(f'{n} is not in range {self}')

        return Span(self.min, n - 1), Span(n, self.max)


def combos(workflows: dict[str | Any, list[Step]], dest: str, spans: tuple[Span]) -> int:
    if dest == 'A':
        return prod([len(span) for span in spans])
    elif dest == 'R':
        return 0

    flow = workflows[dest]
    span_index = {'x': 0, 'm': 1, 'a': 2, 's': 3}

    for step in flow[:-1]:
        span = spans[span_index[step.var_name]]

        if (step.op == '<' and span.min >= step.value) or (step.op == '>' and span.max <= step.value):
            continue  # rule does not apply in this case
        elif (step.op == '<' and span.max < step.value) or (step.op == '>' and span.min > step.value):
            return combos(step.dest, spans)
        elif step.op == '<':
            splits = span.split(step.value)
            lower_span = [*spans]
            upper_span = [*spans]
            lower_span[span_index[step.var_name]] = splits[0]
            upper_span[span_index[step.var_name]] = splits[1]

            return combos(workflows, step.dest, tuple(lower_span)) + combos(workflows, dest, tuple(upper_span))
        elif step.op == '>':
            splits = span.split(step.value + 1)
            lower_span = [*spans]
            upper_span = [*spans]
            lower_span[span_index[step.var_name]] = splits[0]
            upper_span[span_index[step.var_name]] = splits[1]

            return combos(workflows, dest, tuple(lower_span)) + combos(workflows, step.dest, tuple(upper_span))

    return combos(workflows, flow[-1].dest, tuple(spans))


def part2(data: str) -> int:
    rules, _ = parse_input(data)

    workflows = parse_workflows(rules)

    return combos(workflows, 'in', (Span(1, 4000), Span(1, 4000), Span(1, 4000), Span(1, 4000)))


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.data = f.read()

    def test_part1(self):
        res = part1(self.data)
        self.assertEqual(19114, res)

    def test_part2(self):
        res = part2(self.data)
        self.assertEqual(167409079868000, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = f.read()

    print(f'part1 -> {part1(data)}')
    print(f'part2 -> {part2(data)}')
