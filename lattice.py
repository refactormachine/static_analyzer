from copy import deepcopy


class LatticeState(object):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "value={value}".format(value=self.value)


class PcpLatticeState(LatticeState):
    def __init__(self, value):
        LatticeState.__init__(self, value)


class TopLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self, "TOP")


class BottomLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self, "BOTTOM")


class Lattice(object):
    def __init__(self, variable_name, initial_state=None):
        if not initial_state:
            initial_state = BottomLatticeState()
        self.variable_name = variable_name
        self.state = initial_state

    def __repr__(self):
        return "GENERIC_LATTICE_{variable}: {state}".format(variable=self.variable_name, state=self.state)


class EvenPcpLatticeState(PcpLatticeState):
    def __init__(self):
        PcpLatticeState.__init__(self, "EVEN")


class OddPcpLatticeState(PcpLatticeState):
    def __init__(self):
        PcpLatticeState.__init__(self, "ODD")


class EvenConstantPcpLatticeState(EvenPcpLatticeState):
    def __init__(self, value):
        PcpLatticeState.__init__(self, value)


class OddConstantPcpLatticeState(OddPcpLatticeState):
    def __init__(self, value):
        PcpLatticeState.__init__(self, value)


class PcpLattice(Lattice):
    def __init__(self, variable_name, initial_state=None):
        if not initial_state:
            initial_state = BottomLatticeState()
        Lattice.__init__(self, variable_name, initial_state)

    def __repr__(self):
        return "PARITY_CONSTANT_PROPAGATION_LATTICE__{variable}@{state}". \
            format(variable=self.variable_name, state=self.state)

    @staticmethod
    def get_constant_state(n):
        if n % 2 == 0:
            return EvenConstantPcpLatticeState(n)
        else:
            return OddConstantPcpLatticeState(n)

    @staticmethod
    def unify_many(states):
        # type: (list[PcpLatticeState]) -> PcpLatticeState
        if len(states) == 1:
            return deepcopy(states[0])
        state_1 = states.pop()
        state_2 = states.pop()
        temporary_unified_state = PcpLattice.unify_two(state_1, state_2)
        states.append(temporary_unified_state)
        return PcpLattice.unify_many(states)

    @staticmethod
    def unify_two(state_1, state_2):
        if type(state_1) is BottomLatticeState:
            return deepcopy(state_2)
        if type(state_2) is BottomLatticeState:
            return deepcopy(state_1)
        # not bottom
        if type(state_1) is TopLatticeState:
            return deepcopy(state_1)
        if type(state_2) is TopLatticeState:
            return deepcopy(state_2)
        # not bottom or top
        if isinstance(state_1, OddPcpLatticeState) and isinstance(state_2, EvenPcpLatticeState):
            return TopLatticeState()
        if isinstance(state_1, EvenPcpLatticeState) and isinstance(state_2, OddPcpLatticeState):
            return TopLatticeState()
        # same parity
        if type(state_1) is EvenConstantPcpLatticeState and type(state_2) is EvenConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return deepcopy(state_1)
            else:
                return EvenPcpLatticeState()
        if type(state_1) is OddConstantPcpLatticeState and type(state_2) is OddConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return deepcopy(state_1)
            else:
                return OddPcpLatticeState()
        # same parity, at least one isn't constant
        if type(state_1) is EvenPcpLatticeState or type(state_2) is EvenPcpLatticeState:
            return EvenPcpLatticeState()
        elif type(state_1) is OddPcpLatticeState or type(state_2) is OddPcpLatticeState:
            return OddPcpLatticeState()
        raise Exception(state_1, state_2)

    @staticmethod
    def join_many(states):
        if len(states) == 1:
            return states[0]
        state_1 = states.pop()
        state_2 = states.pop()
        temporary_joined_state = PcpLattice.join_two(state_1, state_2)
        states.append(temporary_joined_state)
        return PcpLattice.join_many(states)

    @staticmethod
    def join_two(state_1, state_2):
        if BottomLatticeState in [type(state_1), type(state_2)]:
            return BottomLatticeState()
        # not bottom
        if type(state_1) is TopLatticeState:
            return state_2
        if type(state_2) is TopLatticeState:
            return state_1
        # not bottom or top
        if isinstance(state_1, OddPcpLatticeState) and isinstance(state_2, EvenPcpLatticeState):
            return BottomLatticeState()
        if isinstance(state_1, EvenPcpLatticeState) and isinstance(state_2, OddPcpLatticeState):
            return BottomLatticeState()
        # same parity
        if type(state_1) is EvenConstantPcpLatticeState and type(state_2) is EvenConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return BottomLatticeState()
        if type(state_1) is OddConstantPcpLatticeState and type(state_2) is OddConstantPcpLatticeState:
            if state_1.value == state_2.value:
                return state_1
            else:
                return BottomLatticeState()
        # same parity, at least one isn't constant
        if type(state_1) is EvenConstantPcpLatticeState or type(state_1) is OddConstantPcpLatticeState:
            return state_1
        if type(state_2) is EvenConstantPcpLatticeState or type(state_2) is OddConstantPcpLatticeState:
            return state_2
        if type(state_1) is EvenPcpLatticeState or type(state_1) is OddPcpLatticeState:
            return state_1
        raise Exception(state_1, state_2)


def are_lattices_equal(lattices_1, lattices_2):
    variables = lattices_1.keys()
    return all([lattices_1[v].state.value == lattices_2[v].state.value for v in variables])