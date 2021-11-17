# Collects results and produces structured data for Slog vs. Souffle
# vs. Radlog. This code assumes that a specific version of radlog
# exists at `radlog` (e.g., mounted via Docker).
import subprocess

problems = ["tc_tjoing"] #,"3-clique","5-clique"]

input_graphs = ["ring_6000"]

compiletime = {}
compilememory = {}

## Assume GNU time 
def run_souffle_expt(problem, input_graph, cores,id):
    print("ID {}".format(id))
    proc = subprocess.Popen(["/usr/bin/time","-f","%E,%M","./souffle/build/src/souffle","--dl-program=out","-j{}".format(cores),"-F","input-graphs/{}".format(input_graph),"{}/program.dl".format(problem)], stderr=subprocess.PIPE)
    res = proc.communicate()
    ans = str(res[1].decode('utf-8'))
    time = ans.split(",")[0]
    rss = ans.split(",")[1]
    f = open("results/{}_{}_{}_{}_compilation_memory.out".format(problem,input_graph,cores,id),'w')
    f.write(rss)
    compilememory[(problem,input_graph,cores,id)] = rss
    f.close()
    f = open("results/{}_{}_{}_{}_compilation_time.out".format(problem,input_graph,cores,id),'w')
    f.write(time)
    compiletime[(problem,input_graph,cores,id)] = time
    f.close()
    print("Compilation took {} and {}MB".format(time,int(rss)/1024))
    proc = subprocess.Popen(["/usr/bin/time","-f","%E,%M","./out","-j{}".format(cores)], stderr=subprocess.PIPE)
    ans = str(res[1].decode('utf-8'))
    time = ans.split(",")[0]
    rss = ans.split(",")[1]
    f = open("results/{}_{}_{}_{}_souffle_memory.out".format(problem,input_graph,cores,id),'w')
    f.write(rss)
    compilememory[(problem,input_graph,cores,id)] = rss
    f.close()
    f = open("results/{}_{}_{}_{}_souffle_time.out".format(problem,input_graph,cores,id),'w')
    f.write(time)
    compiletime[(problem,input_graph,cores,id)] = time
    f.close()
    print("Run took {} and {}MB".format(time,int(rss)/1024))

# bin/spark-submit --driver-memory 70G --class edu.ucla.cs.wis.bigdatalog.spark.test.DatalogTest --master local[*] ./datalog/target/spark-datalog_2.11-2.0.3-SNAPSHOT.jar
# change files test_datalog.txt
def run_radlog_expt(problem,input_graph,cores,id):
    return

def average(problem,graph,cores,id_list):
    compiletimesum = 0
    for key in compiletime:
        if (key[0] == problem and key[1] == graph
            and key[2] == cores and key[3] in id_list):
            parts = compiletime[key].split(":")
            hours = 0
            if len(parts) == 3:
                hours = int(parts[0])
                mins = int(parts[1])
                secs = int(parts[2].split(".")[0])
                secs += float(parts[2].split(".")[1])
            else:
                mins = int(parts[0])
                secs = int(parts[1].split(".")[0])
                secs += float(parts[1].split(".")[1])
    compiletimeavg = compiletimesum/len(id_list)
    return compiletimeavg

def run_problem(problem):
    for graph in input_graphs:
        run_souffle_expt(problem,graph,4,"id0")
        run_souffle_expt(problem,graph,4,"id1")
        run_souffle_expt(problem,graph,4,"id2")
        avg = average(problem,graph,4,["id0","id1","id2"])
        print("Avg compiletime: {}".format(avg))

def __main__():
    for problem in problems:
        print("Running problem {}".format(problem))
        run_problem(problem)

__main__()
