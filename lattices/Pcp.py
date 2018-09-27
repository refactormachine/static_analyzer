from Lattice import Lattice, LatticeState, BottomLatticeState, TopLatticeState


class PcpLatticeState(LatticeState):
    def __init__(self, lattice, variable_name, value):
        LatticeState.__init__(self, lattice, variable_name, value)


class EvenPcpLatticeState(PcpLatticeState):
    def __init__(self, lattice, variable_name):
        PcpLatticeState.__init__(self, lattice, variable_name, "EVEN")


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
        Lattice.__init__(self, variable_name)

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

    def unify_many(self, states):
        if len(states) == 1:
            return states[0]
        state_1 = states.pop()
        state_2 = states.pop()
        temporary_unified_state = self.unify_two(state_1, state_2)
        states.append(temporary_unified_state)
        return self.unify_many(states)

    def unify_two(self, state_1, state_2):
        assert state_1.lattice == state_2.lattice

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
            return TopLatticeState(self, self.variable_name)
        if isinstance(state_1, EvenPcpLatticeState) and isinstance(state_2, OddPcpLatticeState):
            return TopLatticeState(self, self.variable_name)
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
