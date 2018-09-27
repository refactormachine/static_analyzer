
class LatticeState(object):
    def __init__(self, lattice, variable_name, value):
        self.lattice = lattice
        self.variable_name = variable_name
        self.value = value

    def __repr__(self):
        return "{variable_name}={lattice}.{value}".\
            format(variable_name=self.variable_name, lattice=self.lattice, value=self.value)


class TopLatticeState(LatticeState):
    def __init__(self, lattice, variable_name):
        LatticeState.__init__(self, lattice, variable_name, "TOP")


class BottomLatticeState(LatticeState):
    def __init__(self, lattice, variable_name):
        LatticeState.__init__(self, lattice, variable_name, "BOTTOM")


class Lattice(object):
    def __init__(self, variable_name):
        self.variable_name = variable_name

    def get_top(self):
        return TopLatticeState(self, self.variable_name)

    def get_bottom(self):
        return BottomLatticeState(self, self.variable_name)

    def __repr__(self):
        return "GENERIC_LATTICE_{variable}".format(variable=self.variable_name)

    def unify_many(self, variable_states):
        return self.get_top()
