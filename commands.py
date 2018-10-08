import re
from copy import deepcopy

from lattice import TopLatticeState, BottomLatticeState, EvenPcpLatticeState, OddPcpLatticeState, \
    EvenConstantPcpLatticeState, OddConstantPcpLatticeState, PcpLattice


class Command(object):
    def apply(self, lattices):
        # type: (dict[Lattice]) -> dict[Lattice]
        pass


class SkipCommand(Command):
    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        return c_lattices


class VariableAssignmentCommand(Command):
    def __init__(self, lhs_variable, rhs_variable):
        self.lhs_variable = lhs_variable
        self.rhs_variable = rhs_variable

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        c_lattices[self.lhs_variable].state = lattices[self.rhs_variable].state
        return c_lattices


class ConstantAssignmentCommand(Command):
    def __init__(self, lhs_variable, constant_value):
        self.lhs_variable = lhs_variable
        self.constant_value = constant_value

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        c_lattices[self.lhs_variable].state = PcpLattice.get_constant_state(self.constant_value)
        return c_lattices


class ArbitraryAssignmentCommand(Command):
    def __init__(self, lhs_variable):
        self.lhs_variable = lhs_variable

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        c_lattices[self.lhs_variable].state = TopLatticeState()
        return c_lattices


class AdditionCommand(Command):
    def __init__(self, lhs_variable, rhs_variable, constant_value):
        self.lhs_variable = lhs_variable
        self.rhs_variable = rhs_variable
        self.constant_value = constant_value

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        rhs_lattice_state = c_lattices[self.rhs_variable].state
        if type(rhs_lattice_state) in [TopLatticeState, BottomLatticeState]:
            c_lattices[self.lhs_variable].state = rhs_lattice_state
        elif type(rhs_lattice_state) in [EvenConstantPcpLatticeState, OddConstantPcpLatticeState]:
            state_constant_value = int(rhs_lattice_state.value)
            c_lattices[self.lhs_variable].state = \
                PcpLattice.get_constant_state(state_constant_value + self.constant_value)
        # Even or Odd
        elif self.constant_value % 2 == 0:
            c_lattices[self.lhs_variable].state = rhs_lattice_state
        else:
            if type(rhs_lattice_state) is EvenPcpLatticeState:
                c_lattices[self.lhs_variable].state = OddPcpLatticeState()
            elif type(rhs_lattice_state) is OddPcpLatticeState:
                c_lattices[self.lhs_variable].state = EvenPcpLatticeState()
        return c_lattices


def parse_raw_conditions(raw_conditions):
    parsed_conditions = map(lambda l: zip(l.lower().split(" ")[0::2], l.split(" ")[1::2]),
                            raw_conditions[1:-1].split(")("))

    def check_condition(lattices, condition):
        variable_state_value = lattices[condition[1]].state
        if isinstance(variable_state_value, TopLatticeState):
            return True
        if isinstance(variable_state_value, BottomLatticeState):
            return False
        desired_parity_class = OddPcpLatticeState if condition[0] == "odd" else EvenPcpLatticeState
        return isinstance(variable_state_value, desired_parity_class)

    def check_and_conditions(lattices, and_condition):
        return all([check_condition(lattices, condition) for condition in and_condition])

    def check_or_conditions(lattices):
        return any([check_and_conditions(lattices, and_condition) for and_condition in parsed_conditions])

    return check_or_conditions


class AssertCommand(Command):
    def __init__(self, raw_conditions):
        self.raw_conditions = raw_conditions
        self.check_conditions = parse_raw_conditions(self.raw_conditions)

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        if not self.check_conditions(c_lattices):
            raise AssertionError("Assertion not met:\n{raw_condition}\n{lattices}". \
                                 format(raw_condition=self.raw_conditions, lattices=lattices))
        return c_lattices


class AssumeCommand(Command):
    def __init__(self, raw_assumption):
        self.raw_assumption = raw_assumption

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        m = re.match("^true$", self.raw_assumption, re.IGNORECASE)
        if m:
            return c_lattices
        m = re.match("^false$", self.raw_assumption, re.IGNORECASE)
        if m:
            for variable in c_lattices.keys():
                c_lattices[variable].state = BottomLatticeState()
            return c_lattices
        m = re.match("^(\w+)\s*=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_lattice = c_lattices[lhs_variable]  # type: PcpLattice
            rhs_constant_value = int(m.group(2))
            lhs_lattice_constant_state = lhs_variable_lattice.get_constant_state(rhs_constant_value)
            c_lattices[lhs_variable].state = \
                PcpLattice.join_many([lhs_variable_lattice.state, lhs_lattice_constant_state])
            return c_lattices
        m = re.match("^(\w+)\s*=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattices[lhs_variable].state
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattices[rhs_variable].state
            c_lattices[lhs_variable].state = \
                PcpLattice.join_many([lhs_variable_state, rhs_variable_state])
            c_lattices[rhs_variable].state = \
                PcpLattice.join_many([rhs_variable_state, lhs_variable_state])
            return c_lattices
        m = re.match("^(\w+)\s*!=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattices[lhs_variable].state
            rhs_constant_value = int(m.group(2))
            lhs_lattice_constant_state = PcpLattice.get_constant_state(rhs_constant_value)
            if type(lhs_variable_state) is not type(lhs_lattice_constant_state):
                return c_lattices
            elif lhs_variable_state.value == rhs_constant_value:
                c_lattices[lhs_variable].state = BottomLatticeState()
                return c_lattices
            else:
                return c_lattices
        m = re.match("^(\w+)\s*!=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattices[lhs_variable].state
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattices[rhs_variable].state
            if type(lhs_variable_state) is type(rhs_variable_state) and \
                    type(lhs_variable_state) in [OddConstantPcpLatticeState, EvenConstantPcpLatticeState]:
                if lhs_variable_state.value == rhs_variable_state.value:
                    c_lattices[lhs_variable].state = BottomLatticeState()
                    c_lattices[rhs_variable].state = BottomLatticeState()
            return c_lattices
        raise Exception("Unknown assumption: {assumption}".format(assumption=self.raw_assumption))


def parse_command_line(raw_command):
    # type: (str) -> Command
    if re.match("skip", raw_command, re.IGNORECASE):
        return SkipCommand()
    m = re.match("^(\w+)\s*:=\s*(\d+)$", raw_command, re.IGNORECASE)
    if m:
        return ConstantAssignmentCommand(m.group(1), int(m.group(2)))
    m = re.match("^(\w+)\s*:=\s*(\w+)$", raw_command, re.IGNORECASE)
    if m:
        return VariableAssignmentCommand(m.group(1), m.group(2))
    m = re.match("^(\w+)\s*:=\s*(\w+)\s*([-+]\s*\d+)$", raw_command, re.IGNORECASE)
    if m:
        return AdditionCommand(m.group(1), m.group(2), int(m.group(3)))
    m = re.match("^(\w+)\s*:=\s*?\?\s*$", raw_command, re.IGNORECASE)
    if m:
        return ArbitraryAssignmentCommand(m.group(1))
    m = re.match("^assert\s*(\(.+\)\s*)+$", raw_command, re.IGNORECASE)
    if m:
        return AssertCommand(m.group(1))
    m = re.match("^assume\s*\(\s*(.*?)\s*\)$", raw_command, re.IGNORECASE)
    if m:
        return AssumeCommand(m.group(1))
    raise Exception("Unknown command: {cmd}".format(cmd=raw_command))