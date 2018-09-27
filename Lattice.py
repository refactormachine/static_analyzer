
class LatticeState(object):
    def __init__(self):
        pass


class TopLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self)


class BottomLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self)


class Lattice(object):
    @staticmethod
    def get_top():
        return TopLatticeState()

    @staticmethod
    def get_bottom():
        return BottomLatticeState()

    @staticmethod
    def unify_many(states):
        # type: ([LatticeState]) -> LatticeState
        pass
