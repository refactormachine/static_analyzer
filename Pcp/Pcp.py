from Lattice import Lattice, LatticeState, BottomLatticeState, TopLatticeState
from Command import *
import re

class PcpLatticeState(LatticeState):
    def __init__(self, variable_name, value):
        LatticeState.__init__(self)
        self.variable_name = variable_name
        self.value = value


class EvenPcpLatticeState(PcpLatticeState):
    def __init__(self, lattice, variable_name):
        PcpLatticeState.__init__(self, variable_name, "EVEN")


class OddPcpLatticeState(PcpLatticeState):
    def __init__(self, lattice, variable_name):
        PcpLatticeState.__init__(self, lattice, variable_name, "ODD")


class EvenConstantPcpLatticeState(EvenPcpLatticeState):
    def __init__(self, lattice, variable_name, value):
        PcpLatticeState.__init__(self, lattice, variable_name, value)


class OddConstantPcpLatticeState(OddPcpLatticeState):
    def __init__(self, lattice, variable_name, value):
        PcpLatticeState.__init__(self, lattice, variable_name, value)


class PcpLattice(Lattice):
    def __init__(self, variable_name):
        Lattice.__init__(self)
        self.variable_name = variable_name

    def __repr__(self):
        return "PARITY_CONSTANT_PROPAGATION_LATTICE_{variable}".format(variable=self.variable_name)

    def get_even_state(self):
        return EvenPcpLatticeState(self, self.variable_name)

    def get_odd_state(self):
        return OddPcpLatticeState(self, self.variable_name)

    def get_constant_state(self, n):
        if n % 2 == 0:
            return EvenConstantPcpLatticeState(self, self.variable_name, n)
        else:
            return OddConstantPcpLatticeState(self, self.variable_name, n)


class CartesianPcpLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self)


class CartesianPcpLattice(Lattice):
    def __init__(self, variables_names):
        Lattice.__init__(self)
        self.variables_names = variables_names
        self.lattices = {variable_name: PcpLattice(variable_name) for variable_name in self.variables_names}

    @staticmethod
    def unify_many(states):
        if len(states) == 1:
            return states[0]
        state_1 = states.pop()
        state_2 = states.pop()
        temporary_unified_state = CartesianPcpLattice.unify_two(state_1, state_2)
        states.append(temporary_unified_state)
        return CartesianPcpLattice.unify_many(states)

    @staticmethod
    def unify_two(self, state_1, state_2):
        # type: (CartesianPcpLatticeState, CartesianPcpLatticeState) -> CartesianPcpLatticeState
        if type(state_1) is BottomLatticeState:
            return state_2
        if type(state_2) is BottomLatticeState:
            return state_1
        # not bottom
        if type(state_1) is TopLatticeState:
            return state_1
        if type(state_2) is TopLatticeState:
            return state_2
        # not bottom or top
        if isinstance(state_1, OddPcpLatticeState) and isinstance(state_2, EvenPcpLatticeState):
            return Lattice.get_top()
        if isinstance(state_1, EvenPcpLatticeState) and isinstance(state_2, OddPcpLatticeState):
            return Lattice.get_top()
        # same parity
        if type(state_1) is EvenConstantPcpLatticeState and type(state_2) is EvenConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return EvenPcpLatticeState(self, self.variable_name)
        if type(state_1) is OddConstantPcpLatticeState and type(state_2) is OddConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return OddPcpLatticeState(self, self.variable_name)
        # same parity, at least one isn't constant
        if type(state_1) is EvenPcpLatticeState or type(state_2) is EvenPcpLatticeState:
            return EvenPcpLatticeState(self, self.variable_name)
        elif type(state_1) is OddPcpLatticeState or type(state_2) is OddPcpLatticeState:
            return OddPcpLatticeState(self, self.variable_name)
        raise Exception(state_1, state_2)

    def join_many(self, states):
        if len(states) == 1:
            return states[0]
        state_1 = states.pop()
        state_2 = states.pop()
        temporary_joined_state = self.join_two(state_1, state_2)
        states.append(temporary_joined_state)
        return self.join_many(states)

    # TODO: fix returning different lattice
    def join_two(self, state_1, state_2):
        assert state_1.lattice == state_2.lattice

        if BottomLatticeState in [type(state_1), type(state_2)]:
            return self.get_bottom()
        # not bottom
        if type(state_1) is TopLatticeState:
            return state_2
        if type(state_2) is TopLatticeState:
            return state_1
        # not bottom or top
        if isinstance(state_1, OddPcpLatticeState) and isinstance(state_2, EvenPcpLatticeState):
            return BottomLatticeState(self, self.variable_name)
        if isinstance(state_1, EvenPcpLatticeState) and isinstance(state_2, OddPcpLatticeState):
            return BottomLatticeState(self, self.variable_name)
        # same parity
        if type(state_1) is EvenConstantPcpLatticeState and type(state_2) is EvenConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return BottomLatticeState(self, self.variable_name)
        if type(state_1) is OddConstantPcpLatticeState and type(state_2) is OddConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return BottomLatticeState(self, self.variable_name)
        # same parity, at least one isn't constant
        if type(state_1) is EvenConstantPcpLatticeState or type(state_1) is OddConstantPcpLatticeState:
            return state_1
        if type(state_2) is EvenConstantPcpLatticeState or type(state_2) is OddConstantPcpLatticeState:
            return state_2
        if type(state_1) is EvenPcpLatticeState or type(state_1) is OddPcpLatticeState:
            return state_1
        raise Exception(state_1, state_2)

class SkipCommand(Command):
    def apply(self, lattices_state):
        return lattices_state


class VariableAssignmentCommand(Command):
    def __init__(self, lhs_variable, rhs_variable):
        self.lhs_variable = lhs_variable
        self.rhs_variable = rhs_variable

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        c_lattice_state[self.lhs_variable].value = lattices_state[self.rhs_variable].value
        return c_lattice_state


class ConstantAssignmentCommand(Command):
    def __init__(self, lhs_variable, constant_value):
        self.lhs_variable = lhs_variable
        self.constant_value = constant_value

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        c_lattice_state[self.lhs_variable]= \
            PcpLattice.get_constant_state(lattices_state[self.lhs_variable].lattice, self.constant_value)
        return c_lattice_state


class ArbitraryAssignmentCommand(Command):
    def __init__(self, lhs_variable):
        self.lhs_variable = lhs_variable

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        c_lattice_state[self.lhs_variable] = PcpLattice.get_top(lattices_state[self.lhs_variable].lattice)
        return c_lattice_state


class AssignAdditionCommand(Command):
    def __init__(self, lhs_variable, rhs_variable, constant_value):
        self.lhs_variable = lhs_variable
        self.rhs_variable = rhs_variable
        self.constant_value = constant_value

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        rhs_lattice_state = lattices_state[self.rhs_variable]
        if type(rhs_lattice_state) in [TopLatticeState, BottomLatticeState]:
            c_lattice_state[self.lhs_variable].value = rhs_lattice_state.value
        elif type(rhs_lattice_state) in [EvenConstantPcpLatticeState, OddConstantPcpLatticeState]:
            state_constant_value = int(rhs_lattice_state.value)
            c_lattice_state[self.lhs_variable] = \
                c_lattice_state[self.lhs_variable].lattice.get_constant_state(state_constant_value + self.constant_value)
        # Even or Odd
        elif self.constant_value % 2 == 0:
            c_lattice_state[self.lhs_variable].value = rhs_lattice_state.value
        else:
            if type(rhs_lattice_state) is EvenPcpLatticeState:
                c_lattice_state[self.lhs_variable] = c_lattice_state[self.lhs_variable].lattice.get_odd_state()
            elif type(rhs_lattice_state) is OddPcpLatticeState:
                c_lattice_state[self.lhs_variable] = c_lattice_state[self.lhs_variable].lattice.get_even_state()
        return c_lattice_state


class AssertCommand(Command):
    def __init__(self, raw_condition):
        self.raw_condition = raw_condition

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        # TODO: parse condition recursively and complete the function
        return c_lattice_state


class AssumeCommand(Command):
    def __init__(self, raw_assumption):
        self.raw_assumption = raw_assumption

    def apply(self, lattices_state):
        c_lattice_state = lattices_state.copy()
        m = re.match("^true$", self.raw_assumption, re.IGNORECASE)
        if m:
            return c_lattice_state
        m = re.match("^false$", self.raw_assumption, re.IGNORECASE)
        if m:
            for variable in c_lattice_state:
                c_lattice_state[variable] = c_lattice_state[variable].lattice.get_bottom()
            return c_lattice_state
        m = re.match("^(\w+)\s*=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_lattice = c_lattice_state[lhs_variable].lattice  # type: PcpLattice
            rhs_constant_value = int(m.group(2))
            lhs_lattice_constant_state = lhs_variable_lattice.get_constant_state(rhs_constant_value)
            c_lattice_state[lhs_variable] = \
                lhs_variable_lattice.join_many([c_lattice_state[lhs_variable], lhs_lattice_constant_state])
            return c_lattice_state
        m = re.match("^(\w+)\s*=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattice_state[lhs_variable]  # type: PcpLatticeState
            lhs_variable_lattice = lhs_variable_state.lattice  # type: PcpLattice
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattice_state[rhs_variable]  # type: PcpLatticeState
            rhs_variable_lattice = rhs_variable_state.lattice  # type: PcpLattice
            c_lattice_state[lhs_variable] = \
                lhs_variable_lattice.join_many([lhs_variable_state, rhs_variable_state])
            c_lattice_state[rhs_variable] = \
                rhs_variable_lattice.join_many([rhs_variable_state, lhs_variable_state])
            return c_lattice_state
        m = re.match("^(\w+)\s*!=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattice_state[lhs_variable]
            lhs_variable_lattice = lhs_variable_state.lattice  # type: PcpLattice
            rhs_constant_value = int(m.group(2))
            lhs_lattice_constant_state = lhs_variable_lattice.get_constant_state(rhs_constant_value)
            if type(lhs_variable_state) is not type(lhs_lattice_constant_state):
                return c_lattice_state
            elif lhs_variable_state.value == rhs_constant_value:
                c_lattice_state[lhs_variable] = lhs_variable_lattice.get_bottom()
                return c_lattice_state
            else:
                return c_lattice_state
        m = re.match("^(\w+)\s*!=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_state = c_lattice_state[lhs_variable]  # type: PcpLatticeState
            lhs_variable_lattice = lhs_variable_state.lattice  # type: PcpLattice
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattice_state[rhs_variable]  # type: PcpLatticeState
            rhs_variable_lattice = rhs_variable_state.lattice  # type: PcpLattice
            if type(lhs_variable_state) is type(rhs_variable_state) and \
                    type(lhs_variable_state) in [OddConstantPcpLatticeState, EvenConstantPcpLatticeState]:
                if lhs_variable_state.value == rhs_variable_state.value:
                    c_lattice_state[lhs_variable] = lhs_variable_lattice.get_bottom()
                    c_lattice_state[rhs_variable] = rhs_variable_lattice.get_bottom()
            return c_lattice_state
        raise Exception("Unknown assumption: {assumption}".format(assumption=self.raw_assumption))