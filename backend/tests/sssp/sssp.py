
import networkx as nx

data_f = open("/home/ubuntu/workspace/dataset/soc-LiveJournal1.txt")
# data_f = open("/home/ubuntu/workspace/slog/backend/tests/sssp/test-input-graph/edge.csv")

g = nx.DiGraph()
for l in data_f:
    g.add_edge(*map(int, l.strip().split("\t")))

sssp_nodes = 0
for i in range(1,10):
    reached_map = nx.shortest_path(g, i)
    sssp_nodes = sssp_nodes + len(reached_map.keys())
    for k, v in reached_map.items():
        print(f"{k} {i} {len(v)-1}") 

print(sssp_nodes)
