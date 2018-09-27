import re
import sys
from Lattice import Lattice, LatticeState, BottomLatticeState, TopLatticeState
from Pcp.Pcp import CartesianPcpLattice


def read_snippet(snippet_name):
    with open("./snippets/{}".format(snippet_name), "rb") as f:
        code = f.read().splitlines()
        code = filter(lambda l: not re.match(r"^\s*#", l), code)
        code = filter(lambda l: not re.match(r"^\s*(\r\n)*$", l), code)
    return code


def parse_variables_from_snippet(snippet_code):
    # type: ([str]) -> [str]
    variables_line = snippet_code[0]
    return variables_line.split()


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
    def __init__(self, node_id, entering_nodes, leaving_edges, lattice_state):
        self.node_id = node_id
        self.leaving_edges = leaving_edges
        self.entering_nodes = entering_nodes
        self.lattice_state = lattice_state  # type: LatticeState

    def __repr__(self):
        s = "\nNode#{node_id}: {lattice_state}\n".\
            format(node_id=self.node_id, lattice_state=self.lattice_state)
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


def build_graph(snippet, lattice):
    # type: ([str], Lattice) -> Graph
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
        node = Node(node_id, entering_nodes, leaving_edges, lattice.get_bottom())
        nodes[node_id] = node
    root_node_id = min(nodes.keys())
    root_node = nodes[root_node_id]
    root_node.lattice_state = lattice.get_top()
    return Graph(nodes, root_node_id, edges)


def unify_nodes_states(nodes_states):
    unified_states = {}
    for variable_name, lattice_state in nodes_states[0].items():
        current_lattice = lattice_state.lattice # type: Lattice
        variable_states = [node_state[variable_name] for node_state in nodes_states]
        unified_states[variable_name] = current_lattice.unify_many(variable_states)
    return unified_states


def run_chaotic_iteration_algorithm(graph, lattice):
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
            updated_lattice_state = edge.command.apply(source_node.lattice_state)
            updated_nodes_lattices_state.append(updated_lattice_state)
        current_node_updated_lattice_state = lattice.unify_many(updated_nodes_lattices_state)
        if current_node_updated_lattice_state != current_node.lattices_state:
            current_node.lattices_state = current_node_updated_lattice_state
            working_list.update([edge.destination_id for edge in current_node.leaving_edges])


def is_assert_edge(edge):
    return is_assert_raw_command(edge.raw_command)


def is_assert_raw_command(raw_command):
    return re.match("^assert\s*\(", raw_command, re.IGNORECASE)


def main(snippet_name):
    snippet = read_snippet(snippet_name)
    variables = parse_variables_from_snippet(snippet)
    lattice = CartesianPcpLattice(variables)
    graph = build_graph(snippet, lattice)
    run_chaotic_iteration_algorithm(graph, lattice)
    print graph
    asserts_edges = filter(is_assert_edge, graph.edges)
    print "==========ASSERTS=========="
    print map(lambda edge: (graph.get_node_by_id(edge.source_id).lattice_state.value(), edge.raw_command), asserts_edges)


if __name__ == "__main__":
    main(sys.argv[1])
