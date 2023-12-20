#!/usr/bin/env python3
import collections
import enum
import itertools
import math
import sys
import unittest
from dataclasses import dataclass, field


class ComponentType(enum.StrEnum):
    FLIP_FLOP = '%'
    CONJUNCTION = '&'
    BROADCASTER = 'X'
    UNTYPED = ''


@dataclass
class Component:
    name: str
    comp_type: ComponentType
    is_on: bool = False
    inputs: list['Component'] = None
    outputs: list['Component'] = None

    def __hash__(self):
        return hash(self.name)


@dataclass
class Conjunction(Component):
    previous_pulses: dict['Component', 'Pulse'] = field(default_factory=dict)

    def setup(self):
        for input in self.inputs:
            self.previous_pulses[input] = Pulse.LOW

    def all_high(self) -> bool:
        return all(pulse == Pulse.HIGH for pulse in self.previous_pulses.values())

    def update(self, comp: Component, pulse: 'Pulse'):
        self.previous_pulses[comp] = pulse

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return f'{self.name} -> {self.previous_pulses}'


class Pulse(enum.IntEnum):
    LOW = 0
    HIGH = 1


def parse(lines: list[str]) -> dict[str, Component]:
    connections = {}
    components = {}

    for line in lines:
        type_name_name, sends_to = line.split(' -> ')

        if type_name_name[0] in [ComponentType.FLIP_FLOP, ComponentType.CONJUNCTION]:
            comp_type = ComponentType(type_name_name[0])
            name = type_name_name[1:]
        else:
            comp_type = ComponentType.BROADCASTER
            name = type_name_name

        if comp_type == ComponentType.CONJUNCTION:
            comp = Conjunction(name, comp_type)
        else:
            comp = Component(name, comp_type)

        components[name] = comp
        connections[name] = sends_to.split(', ')

    # find any output components that aren't in the list yet
    for conns in connections.values():
        for conn in conns:
            if conn not in components:
                components[conn] = Component(conn, ComponentType.UNTYPED)

    for name, comp in components.items():
        if name in connections:
            comp.outputs = [components[conn] for conn in connections[name]]
            comp.inputs = [components[conn] for conn in connections.keys() if name in connections[conn]]

    for conjunctions in [comp for comp in components.values() if comp.comp_type == ComponentType.CONJUNCTION]:
        conjunctions.setup()

    return components


def press_the_button(circuit: dict[str, Component], hits: dict[str, int] = None, iteration: int = 1) -> dict[
    Pulse, int]:
    pulse_counts: dict[Pulse, int] = collections.defaultdict(int)
    pulse_counts[Pulse.LOW] = 1

    queue = collections.deque()
    queue.append((None, circuit['broadcaster'], Pulse.LOW))

    while queue:
        input_comp, comp, pulse = queue.popleft()

        if comp.comp_type == ComponentType.BROADCASTER:
            for output in comp.outputs:
                queue.append((comp, output, pulse))
                pulse_counts[pulse] += 1
        elif comp.comp_type == ComponentType.FLIP_FLOP:
            if pulse == Pulse.HIGH:
                # flip_flops always ignore HIGH pulses
                continue
            if comp.is_on:
                comp.is_on = False
                for output in comp.outputs:
                    queue.append((comp, output, Pulse.LOW))
                    pulse_counts[Pulse.LOW] += 1
            else:
                comp.is_on = True
                for output in comp.outputs:
                    queue.append((comp, output, Pulse.HIGH))
                    pulse_counts[Pulse.HIGH] += 1
        elif comp.comp_type == ComponentType.CONJUNCTION:
            comp.update(input_comp, pulse)

            output_pulse = Pulse.LOW if comp.all_high() else Pulse.HIGH

            for output in comp.outputs:
                queue.append((comp, output, output_pulse))
                pulse_counts[output_pulse] += 1

            if hits is not None and comp.all_high() and comp.name not in hits:
                hits[comp.name] = iteration

    return pulse_counts


def part1(lines: list[str]) -> int:
    circuit = parse(lines)

    totals = collections.defaultdict(int)

    for _ in range(1000):
        pulse_counts = press_the_button(circuit)
        for pulse, count in pulse_counts.items():
            totals[pulse] += count

    return totals[Pulse.HIGH] * totals[Pulse.LOW]


def part2(lines: list[str]) -> int:
    circuit = parse(lines)

    hits = collections.defaultdict(int)

    for press_count in itertools.count(start=1):
        press_the_button(circuit, hits, press_count)
        press_count += 1

        conjunctions = [comp for comp in circuit.values() if comp.comp_type == ComponentType.CONJUNCTION]

        missing = [c.name for c in conjunctions if c.name not in hits]

        # this assumes that if there's only one conjunction that hasn't tripped,
        # then it must be the one right before `rx`, so we can stop
        if len(missing) == 1:
            break

    # the LCM of all how long it took each conjunction to trip
    # will tell us how long it takes for all of them to trip at the same time
    return math.lcm(*hits.values())


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().strip().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(32000000, res)

    def test_part1_5(self):
        with open('input2.txt') as f:
            self.lines = f.read().strip().splitlines()

        res = part1(self.lines)
        self.assertEqual(11687500, res)

    def test_part2(self):
        with open('input1.txt') as f:
            self.lines = f.read().strip().splitlines()

        res = part2(self.lines)
        self.assertEqual(11687500, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
