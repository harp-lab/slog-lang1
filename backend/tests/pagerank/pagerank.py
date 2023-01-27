"""
generate a test graph and pagerank ground truth for testing

"""

import networkx as nx

bag = nx.barabasi_albert_graph(60, 41).to_directed()
pr = nx.pagerank(bag)

with open("test-graph/edge.fasts", "w+") as edge_f:
    for f, t in bag.edges:
        edge_f.write(f"{f}\t{t}\n")

with open("ground_truth", "w+") as truth_f:
    for node, val in pr.items():
        truth_f.write(f"{node}\t{val:.4f}\n")

print("done!")
