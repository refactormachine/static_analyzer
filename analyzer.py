import itertools
import re
import sys
from copy import deepcopy


class LatticeState(object):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return "value={value}".format(value=self.value)


class TopLatticeState(LatticeState):
    def __init__(self):
        LatticeState.__init__(self, "T")


class DcpLatticeState(LatticeState):
    def __init__(self, value):
        LatticeState.__init__(self, value)

    def __repr__(self):
        return self.value


class Lattice(object):
    def __init__(self, variable_name, initial_state=None):
        if not initial_state:
            initial_state = self.get_bottom_state()
        self.variable_name = variable_name
        self.state = initial_state

    def get_bottom_state(self):
        raise NotImplementedError()

    def get_top_state(self):
        raise NotImplementedError()

    def unify_many(self, states):
        raise NotImplementedError()

    def join_many(self, states):
        raise NotImplementedError()

    def get_state_with_value(self, value):
        raise NotImplementedError()

    def __repr__(self):
        return "GENERIC_LATTICE_{variable}: {state}".format(variable=self.variable_name, state=self.state)


class DcpLattice(Lattice):
    def __init__(self, variable_name, initial_state=None):
        Lattice.__init__(self, variable_name, initial_state)

    def __repr__(self):
        return "DISJUNCTIVE_CONSTANT_PROPAGATION_LATTICE__{variable}{state}". \
            format(variable=self.variable_name, state="{" + " V ".join(
            ["{var}={val}".format(var=self.variable_name, val=e) for e in self.state.value]) + "}")

    def get_top_state(self):
        return TopLatticeState()

    def get_bottom_state(self):
        return DcpLatticeState(set())

    def unify_many(self, states):
        if any([isinstance(state, TopLatticeState) for state in states]):
            return TopLatticeState()
        return DcpLatticeState(set.union(*[state.value for state in states]))

    def join_many(self, states):
        if states == [TopLatticeState()]:
            return [TopLatticeState()]
        return self.get_state_with_value(
            set.intersection(*[state.value for state in states if not isinstance(state, TopLatticeState)]))

    def get_state_with_value(self, value):
        return DcpLatticeState(value)


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
        c_lattices[self.lhs_variable].state = c_lattices[self.lhs_variable].get_state_with_value({self.constant_value})
        return c_lattices


class ArbitraryAssignmentCommand(Command):
    def __init__(self, lhs_variable):
        self.lhs_variable = lhs_variable

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        c_lattices[self.lhs_variable].state = c_lattices[self.lhs_variable].get_top_state()
        return c_lattices


class AdditionCommand(Command):
    def __init__(self, lhs_variable, rhs_variable, constant_value):
        self.lhs_variable = lhs_variable
        self.rhs_variable = rhs_variable
        self.constant_value = constant_value

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        rhs_lattice = c_lattices[self.rhs_variable]
        rhs_lattice_state = rhs_lattice.state
        lhs_lattice = c_lattices[self.lhs_variable]  # type: DcpLattice
        if isinstance(rhs_lattice_state, TopLatticeState):
            lhs_lattice.state = lhs_lattice.get_top_state()
        else:
            lhs_lattice.state = lhs_lattice.get_state_with_value({e + self.constant_value for e in rhs_lattice_state.value})
        return c_lattices


def parse_raw_conditions(raw_conditions):
    parsed_or_conditions = map(
        lambda l: map(lambda s: s.split(), re.match("^sum\s+(.+?)\s*=\s*sum\s+(.+)$", l, re.IGNORECASE).groups()),
        raw_conditions[1:-1].split(")("))

    def calculate_condition_vector(variables, condition_part):
        constant = sum([int(e) for e in filter(lambda e: re.match("^-?\d+$", e), condition_part)])
        part_variables = filter(lambda e: re.match("^[^\d-].*$", e), condition_part)
        variables_count = {e: part_variables.count(e) for e in variables}
        return {"variables_count": variables_count, "constant": constant}

    def calculate_difference_vector(variables, lhs_elements, rhs_elements):
        lhs_vector = calculate_condition_vector(variables, lhs_elements)
        rhs_vector = calculate_condition_vector(variables, rhs_elements)

        constants_difference = rhs_vector["constant"] - lhs_vector["constant"]
        variables_count_difference = {
        variable: lhs_vector["variables_count"][variable] - rhs_vector["variables_count"][variable]
        for variable in variables}
        return {"variables_count": variables_count_difference, "constant": constants_difference}

    def is_value_possible(value, variables_count, lattices):
        relevant_lattices = filter(lambda (var, lattice): variables_count[var] != 0, lattices.items())
        relevant_variables_states = {variable: lattice.state for variable, lattice in relevant_lattices}
        if any([isinstance(state, TopLatticeState) for state in relevant_variables_states.values()]):
            return True
        # no top states
        if any([state.value == {} for state in relevant_variables_states.values()]):
            return False
        optional_variables_values = [[v*variables_count[var] for v in state.value]
                                     for var, state in relevant_variables_states.items()]
        optional_values = {sum(i) for i in itertools.product(*optional_variables_values)}
        return value in optional_values

    def check_condition(lattices, condition):
        variables = lattices.keys()
        lhs_elements = condition[0]
        rhs_elements = condition[1]
        difference_vector = calculate_difference_vector(variables, lhs_elements, rhs_elements)
        return is_value_possible(difference_vector["constant"], difference_vector["variables_count"], lattices)

    def check_or_conditions(lattices):
        return any([check_condition(lattices, and_condition) for and_condition in parsed_or_conditions])

    return check_or_conditions


class AssertCommand(Command):
    def __init__(self, raw_conditions):
        self.raw_conditions = raw_conditions
        self.check_conditions = parse_raw_conditions(self.raw_conditions)

    def apply(self, lattices):
        c_lattices = deepcopy(lattices)
        if not self.check_conditions(c_lattices):
            raise AssertionError("Assertion not met:\n{raw_condition}\n{lattices}".
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
                c_lattices[variable].state = c_lattices[variable].get_bottom_state()
            return c_lattices
        m = re.match("^(\w+)\s*=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_variable_lattice = c_lattices[lhs_variable]  # type: Lattice
            rhs_constant_value = int(m.group(2))
            lhs_constant_state = lhs_variable_lattice.get_state_with_value({rhs_constant_value})
            lhs_variable_lattice.state = \
                lhs_variable_lattice.join_many([lhs_variable_lattice.state, lhs_constant_state])
            return c_lattices
        m = re.match("^(\w+)\s*=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_lattice = c_lattices[lhs_variable]
            lhs_variable_state = lhs_lattice.state
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattices[rhs_variable].state
            lhs_lattice.state = \
                lhs_lattice.join_many([lhs_variable_state, rhs_variable_state])
            c_lattices[rhs_variable].state = \
                lhs_lattice.join_many([rhs_variable_state, lhs_variable_state])
            return c_lattices
        m = re.match("^(\w+)\s*!=\s*(\d+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_lattice = c_lattices[lhs_variable]  # type: Lattice
            lhs_variable_state = lhs_lattice.state
            rhs_constant_value = int(m.group(2))
            if isinstance(lhs_variable_state, TopLatticeState):
                return c_lattices
            lhs_variable_state.value.difference_update({rhs_constant_value})
            return c_lattices
        m = re.match("^(\w+)\s*!=\s*(\w+)$", self.raw_assumption, re.IGNORECASE)
        if m:
            lhs_variable = m.group(1)
            lhs_lattice = c_lattices[lhs_variable]
            lhs_variable_state = lhs_lattice.state
            rhs_variable = m.group(2)
            rhs_variable_state = c_lattices[rhs_variable].state
            if (not isinstance(lhs_variable_state, TopLatticeState)) and \
                    (not isinstance(rhs_variable_state, TopLatticeState)):
                if len(lhs_variable_state.value) == 1:
                    rhs_variable_state.value.difference_update(lhs_variable_state.value)
                if len(rhs_variable_state.value) == 1:
                    lhs_variable_state.value.difference_update(rhs_variable_state.value)
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
    try:
        return Edge(*m.groups())
    except Exception as e:
        print code_line
        raise e


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
            format(node_id=self.node_id, lattices=self.lattices.items())
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
        node = Node(node_id, entering_nodes, leaving_edges, {variable: DcpLattice(variable) for variable in variables})
        nodes[node_id] = node
    root_node_id = min(nodes.keys())
    root_node = nodes[root_node_id]
    return Graph(nodes, root_node_id, edges, variables)


def unify_lattices(incoming_lattices_states):
    # type: (list[dict[str, Lattice]]) -> dict[str, Lattice]
    unified_lattices = {}
    variables = incoming_lattices_states[0].keys()
    for variable in variables:
        variable_states = map(lambda lattices: lattices[variable].state, incoming_lattices_states)
        unified_state = incoming_lattices_states[0][variable].unify_many(variable_states)
        unified_lattices[variable] = DcpLattice(variable, initial_state=unified_state)
    return unified_lattices


def run_chaotic_iteration_algorithm(graph):
    # type: (Graph) -> None
    working_list = set(graph.nodes.keys())
    while working_list:
        current_node_id = working_list.pop()
        current_node = graph.get_node_by_id(current_node_id)  # type: Node
        updated_incoming_nodes_lattices = []
        if not current_node.entering_nodes:
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
