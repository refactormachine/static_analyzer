import re
import sys


class LatticeState(object):
    def __init__(self, lattice, variable_name, value):
        self.lattice = lattice
        self.variable_name = variable_name
        self.value = value

    def __repr__(self):
        return "{variable_name}={lattice}.{value}".\
            format(variable_name=self.variable_name, lattice=self.lattice, value=self.value)


class PcpLatticeState(LatticeState):
    def __init__(self, lattice, variable_name, value):
        LatticeState.__init__(self, lattice, variable_name, value)


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
    def apply(self, lattices_state):
        pass


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
        return AssignAdditionCommand(m.group(1), m.group(2), int(m.group(3)))
    m = re.match("^(\w+)\s*:=\s*?\?\s*$", raw_command, re.IGNORECASE)
    if m:
        return ArbitraryAssignmentCommand(m.group(1))
    m = re.match("^assert\s*\((.*?)\)$", raw_command, re.IGNORECASE)
    if m:
        return AssertCommand(m.group(1))
    m = re.match("^assume\s*\(\s*(.*?)\s*\)$", raw_command, re.IGNORECASE)
    if m:
        return AssumeCommand(m.group(1))
    raise Exception("Unknown command: {cmd}".format(cmd= raw_command))


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
    def __init__(self, node_id, entering_nodes, leaving_edges, lattices_states):
        self.node_id = node_id
        self.leaving_edges = leaving_edges
        self.entering_nodes = entering_nodes
        self.lattices_state = lattices_states

    def __repr__(self):
        s = "\nNode#{node_id}: {lattices_states}\n".\
            format(node_id=self.node_id, lattices_states=self.lattices_state.items())
        s += "\tIngoing:\n"
        for edge in self.entering_nodes:
            s += "\t\t{edge}\n".format(edge=edge)
        s += "\tOutgoing:\n"
        for edge in self.leaving_edges:
            s += "\t\t{edge}\n".format(edge=edge)
        return s


class Graph(object):
    def __init__(self, nodes, root_node_id, edges):
        self.nodes = nodes
        self.root_node_id = root_node_id
        self.edges = edges

    def get_root_node(self):
        return self.nodes[self.root_node_id]

    def get_node_by_id(self, node_id):
        return self.nodes[node_id]

    def __repr__(self):
        return str(self.nodes.values())


def build_graph(snippet, lattices):
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
        lattices_bottom_states = dict({(variable, lattice.get_bottom()) for (variable, lattice) in lattices.items()})
        node = Node(node_id, entering_nodes, leaving_edges, lattices_bottom_states)
        nodes[node_id] = node
    root_node_id = min(nodes.keys())
    root_node = nodes[root_node_id]
    root_node.lattices_state = dict({(variable, lattice.get_top()) for (variable, lattice) in lattices.items()})
    return Graph(nodes, root_node_id, edges)


def unify_lattices_states(nodes_states):
    unified_states = {}
    for variable_name, lattice_state in nodes_states[0].items():
        current_lattice = lattice_state.lattice # type: Lattice
        variable_states = [node_state[variable_name] for node_state in nodes_states]
        unified_states[variable_name] = current_lattice.unify_many(variable_states)
    return unified_states


def run_chaotic_iteration_algorithm(graph):
    working_list = {graph.root_node_id}
    while working_list:
        current_node_id = working_list.pop()
        current_node = graph.get_node_by_id(current_node_id)  # type: Node
        updated_nodes_lattices_state = []
        if not current_node.entering_nodes:
            working_list.update([edge.destination_id for edge in current_node.leaving_edges])
            continue
        for edge in current_node.entering_nodes:
            source_node = graph.get_node_by_id(edge.source_id)  # type: Node
            source_lattices_state = source_node.lattices_state
            updated_lattices_state = edge.command.apply(source_lattices_state)
            updated_nodes_lattices_state.append(updated_lattices_state)
        current_node_updated_lattices_state = unify_lattices_states(updated_nodes_lattices_state)
        if current_node_updated_lattices_state != current_node.lattices_state:
            current_node.lattices_state = current_node_updated_lattices_state
            working_list.update([edge.destination_id for edge in current_node.leaving_edges])


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
    lattices = create_lattices_from_variables(variables)
    graph = build_graph(snippet, lattices)  # type: Graph
    run_chaotic_iteration_algorithm(graph)
    print graph
    asserts_edges = filter(is_assert_edge, graph.edges)
    print "==========ASSERTS=========="
    print map(lambda edge: (graph.get_node_by_id(edge.source_id).lattices_state.values(), edge.raw_command), asserts_edges)


if __name__ == "__main__":
    main(sys.argv[1])
