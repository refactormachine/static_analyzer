import re
import sys
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


def read_snippet(snippet_name):
    with open("./snippets/{}".format(snippet_name), "rb") as f:
        code = f.read().splitlines()
        code = filter(lambda l: not re.match(r"^\s*#", l), code)
        code = filter(lambda l: not re.match(r"^\s*(\r\n)*$", l), code)
    return code


def parse_variables_from_snippet(snippet_code):
    variables_line = snippet_code[0]
    return variables_line.split()


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


class AssertCommand(Command):
    def __init__(self, raw_conditions):
        self.raw_conditions = raw_conditions

    def apply(self, lattices):
        # TODO
        c_lattices = deepcopy(lattices)
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


class Edge(object):
    def __init__(self, source, raw_command, destination):
        self.source_id = int(source)
        self.command = parse_command_line(raw_command)
        self.raw_command = raw_command
        self.destination_id = int(destination)

    def __repr__(self):
        return "{source}->{destination}: {cmd}".format(source=self.source_id, cmd=self.raw_command,
                                                       destination=self.destination_id)


def parse_code_line(code_line):
    m = re.match("L(\d+)\s+(.+?)\s+L(\d+)", code_line)
    return Edge(*m.groups())


def parse_edges_from_snippet(snippet):
    raw_code = snippet[1:]
    return map(parse_code_line, raw_code)


class Node(object):
    def __init__(self, node_id, entering_nodes, leaving_edges, lattices):
        self.node_id = node_id
        self.leaving_edges = leaving_edges
        self.entering_nodes = entering_nodes
        self.lattices = lattices

    def __repr__(self):
        s = "\nNode#{node_id}: {lattices}\n". \
            format(node_id=self.node_id, lattices=map(lambda l: l.state, self.lattices.values()))
        s += "\tIngoing:\n"
        for edge in self.entering_nodes:
            s += "\t\t{edge}\n".format(edge=edge)
        s += "\tOutgoing:\n"
        for edge in self.leaving_edges:
            s += "\t\t{edge}\n".format(edge=edge)
        return s


class Graph(object):
    def __init__(self, nodes, root_node_id, edges, variables):
        self.nodes = nodes
        self.root_node_id = root_node_id
        self.edges = edges
        self.variables = variables

    def get_root_node(self):
        return self.nodes[self.root_node_id]

    def get_node_by_id(self, node_id):
        return self.nodes[node_id]

    def __repr__(self):
        return str(self.nodes.values())


def build_graph(snippet, variables):
    edges = parse_edges_from_snippet(snippet)
    edges_by_source = {}
    edges_by_destination = {}
    for edge in edges:
        if edge.source_id not in edges_by_source.keys():
            edges_by_source[edge.source_id] = []
        edges_by_source[edge.source_id].append(edge)
        if edge.destination_id not in edges_by_destination.keys():
            edges_by_destination[edge.destination_id] = []
        edges_by_destination[edge.destination_id].append(edge)
    nodes = {}
    for node_id in set(edges_by_destination.keys() + edges_by_source.keys()):
        entering_nodes = edges_by_destination[node_id] if node_id in edges_by_destination else []
        leaving_edges = edges_by_source[node_id] if node_id in edges_by_source else []
        node = Node(node_id, entering_nodes, leaving_edges, {variable: PcpLattice(variable) for variable in variables})
        nodes[node_id] = node
    root_node_id = min(nodes.keys())
    root_node = nodes[root_node_id]
    for lattice in root_node.lattices.values():
        lattice.state = TopLatticeState()
    return Graph(nodes, root_node_id, edges, variables)


def unify_lattices(incoming_lattices_states):
    # type: (list[dict[str, Lattice]]) -> dict[str, Lattice]
    unified_lattices = {}
    variables = incoming_lattices_states[0].keys()
    for variable in variables:
        variable_states = map(lambda lattices: lattices[variable].state, incoming_lattices_states)
        unified_state = PcpLattice.unify_many(variable_states)
        unified_lattices[variable] = PcpLattice(variable, initial_state=unified_state)
    return unified_lattices


def run_chaotic_iteration_algorithm(graph):
    working_list = {graph.root_node_id}
    while working_list:
        current_node_id = working_list.pop()
        current_node = graph.get_node_by_id(current_node_id)  # type: Node
        updated_incoming_nodes_lattices = []
        if not current_node.entering_nodes:
            working_list.update([edge.destination_id for edge in current_node.leaving_edges])
            continue
        for edge in current_node.entering_nodes:
            edge_node = graph.get_node_by_id(edge.source_id)  # type: Node
            updated_incoming_node_lattices = deepcopy(edge.command.apply(edge_node.lattices))
            updated_incoming_nodes_lattices.append(updated_incoming_node_lattices)
        current_node_updated_lattices = unify_lattices(updated_incoming_nodes_lattices)
        if not are_lattices_equal(current_node.lattices, current_node_updated_lattices):
            current_node.lattices = deepcopy(current_node_updated_lattices)
            working_list.update([edge.destination_id for edge in current_node.leaving_edges])


def are_lattices_equal(lattices_1, lattices_2):
    variables = lattices_1.keys()
    return all([lattices_1[v].state.value == lattices_2[v].state.value for v in variables])


def is_assert_edge(edge):
    return is_assert_raw_command(edge.raw_command)


def is_assert_raw_command(raw_command):
    return re.match("^assert\s*\(", raw_command, re.IGNORECASE)


def create_lattice_for_variable(variable_name):
    return PcpLattice(variable_name)


def create_lattices_from_variables(variables):
    return {variable_name: create_lattice_for_variable(variable_name) for variable_name in variables}


def main(snippet_name):
    snippet = read_snippet(snippet_name)
    variables = parse_variables_from_snippet(snippet)
    graph = build_graph(snippet, variables)  # type: Graph
    run_chaotic_iteration_algorithm(graph)
    print graph
    asserts_edges = filter(is_assert_edge, graph.edges)
    print "==========ASSERTS=========="
    print map(lambda edge: (graph.get_node_by_id(edge.source_id).lattices, edge.raw_command), asserts_edges)


if __name__ == "__main__":
    main(sys.argv[1])
