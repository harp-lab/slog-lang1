#!/usr/bin/env python3

"""
Runs a batch of CFA experiments

USAGE: `python3 run_cfa_expts.py <m/k> <term-size> <polyvariance> <procs> <slog/souffle/both>`
"""

import sys
import os
import random
import string
import base64
import hashlib
import re

verbose = False

# Util

def usage():
    print("USAGE: `python3 run_cfa_expts.py <m/k> <term-size> <polyvariance> <procs> <slog/souffle/both>")
    exit(1)

def exptid(size=8, chars=string.ascii_uppercase + string.digits):
    return 'cfa-expt-' + ''.join(random.choice(chars) for _ in range(size))

def run(cmd):
    if (verbose):
        print(cmd)
    os.system(cmd)

def report(str):
    print(str)

# Prep 
def prep(n,p,expt_id):
    run("racket dvh-worstcase-gen.rkt {} {} >dvh-{}-{}.scm".format(n,p,n,p))
    run("mkdir results/{}".format(expt_id))
    run("racket prog-encoder.rkt dvh-{}-{}.scm".format(n,p))
    run("mv syntax-edb results/{}".format(expt_id))
    run("mv dvh-{}-{}.scm popl-expts/{}".format(n,p,expt_id))

# Slog run/analyze

def analyze_slog(file,mork,n,m,p):
    file1 = open(file, 'r')
    lines = file1.readlines()
    factre = re.compile(r'\d+\.(.+)\.\d+\.table: \{\d+\}. \((\d+) total facts\)')
    resre = re.compile(r'\s*Maximum resident set size \(kbytes\): (\d+)')
    timere = re.compile(r'\s*Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): (\d+:\d+.\d+)')
    factcounts = {}
    max_rss = "--"
    sto_size = "--"
    cf_pts = "--"
    slog_time = "--"
    slog_mem_mb = "--"
    for line in lines:
        r = factre.match(line)
        if r:
            factcounts[r.group(1)] = r.group(2)
        r = resre.match(line)
        if r:
            max_rss = r.group(1)
        r = timere.match(line)
        if r:
            slog_time = r.group(1)
    try:
        sto_size = factcounts["store"]
        cf_pts = int(factcounts["ret"]) + int(factcounts["apply"]) + int(factcounts["eval"])
        slog_mem_mb = int(max_rss) / 1024
    except:
        pass
    report("(Slog -- DVH {}-CFA {}-{}, {} procs) Cf. Pts: {} Sto sz: {} Slog time: {} Slog mem (MB): {}".format(mork,n,m,p,cf_pts,sto_size,slog_time,slog_mem_mb))

def run_slog(mork,n,m,procs,exptid):
    print("(RUN Expt -- Slog {} {} {} {} {})".format(mork,n,m,procs,exptid))
    file = ""
    if mork == "m":
        file = "popl22-slog/code/mcfa-{}.slog".format(m)
    else:
        file = "popl22-slog/code/kcfa-{}.slog".format(m)
    run("cd ../..; ./runslog -ov -v -j {} -f popl22-slog/code/popl-expts/{}/syntax-edb  -cb {} {} >{}.log; mv {}.log popl22-slog/code/popl-expts/{}".format(procs,exptid,file,exptid,exptid,exptid,exptid))
    analyze_slog("popl-expts/{}/{}.log".format(exptid,exptid),mork,n,m,procs)

# Souffle run / analyze

def analyze_souffle(file,mork,n,m,p):
    file1 = open(file, 'r')
    lines = file1.readlines()
    resre = re.compile(r'\s*Maximum resident set size \(kbytes\): (\d+)')
    timere = re.compile(r'\s*Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): (\d+:\d+.\d+)')
    max_rss = "--"
    souffle_time = "--"
    souffle_mem_mb = "--"
    for line in lines:
        r = resre.match(line)
        if r:
            max_rss = r.group(1)
        r = timere.match(line)
        if r:
            souffle_time = r.group(1)
    try:
        souffle_mem_mb = int(max_rss) / 1024
    finally:
        pass
    f = open('sto_sz.csv','r')
    stosz = f.readlines()[0].strip()
    f.close()
    f = open('cf_pts.csv','r')
    cfpts = f.readlines()[0].strip()
    f.close()
    report("(Souffle -- DVH {}-CFA {}-{}, {} procs) Cf. Pts.: {} Sto sz: {} Souffle time: {} Souffle mem (MB): {}".format(mork,n,m,p,cfpts,stosz,souffle_time,souffle_mem_mb))

def run_souffle(mork,n,m,procs,exptid):
    print("(RUN Expt -- Souffle {} {} {} {} {})".format(mork,n,m,procs,exptid))
    file = ""
    if mork == "m":
        file = "mcfa-{}.dl".format(m)
    else:
        file = "kcfa-{}.dl".format(m)
    run("souffle -j {} -c -F popl-expts/{}/syntax-edb -o souffle-bin-{} {} 2>{}-souffle-compile.log".format(procs,exptid,exptid,file,exptid))
    run("/usr/bin/time -v ./souffle-bin-{} -j {} 2>{}-souffle.log".format(exptid,procs,exptid))
    analyze_souffle("{}-souffle.log".format(exptid),mork,n,m,procs)

# Driver

def main(args=sys.argv[1:6],expt_id = exptid()):
    [mork,n,m,procs,which] = args
    try:
        assert(mork == "m" or mork == "k")
        int(n)
        int(m)
        int(procs)
        assert(which == "slog" or which == "souffle" or which == "both")
    except:
        usage()
    prep(n,m,expt_id)
    if which == "slog" or which == "both":
        run_slog(mork,n,m,procs,expt_id)
    if which == "souffle" or which == "both":
        run_souffle(mork,n,m,procs,expt_id)

# Do cross products 
def combos(morks,ns,ms,procs,whichs):
    for mork in morks:
        for n in ns:
            for m in ms:
                for proc_ct in procs:
                    for which in whichs:
                        main([mork,n,m,proc_ct,which], exptid())

# Entrypoint    
if (len(sys.argv) == 6):
    main()
elif (len(sys.argv) == 3 and sys.argv[2] == "souffle"):
    analyze_souffle(sys.argv[1],"k",6,3,8)
elif (len(sys.argv) == 3 and sys.argv[2] == "slog"):
    analyze_slog(sys.argv[1])
elif (len(sys.argv) == 2 and sys.argv[1] == "runall"):
    # Repeat n times
    n = 1
    for i in range(1,n+1):
        #combos(["k"],[8,9,10],[3,4],[3, 6, 12, 24, 48, 96],["both"])
        #combos(["k"],[10],[3],[3,6,12,24,48,96],["slog"])
        #combos(["k"],[10],[3],[45],["slog"])
        #combos(["m"],[50],[5],[3,6,12,24,48,96],["souffle"])
        # Good?
        #combos(["m"],[200],[10],[3,6,12,24,48,96],["slog"])
        # combos(["m"],[400],[15],[24,48,96,3,6,12],["slog"]) segfault
        #combos(["m"],[425],[10],[24],["slog"]) ok 
        #combos(["m"],[450],[10],[24],["slog"]) ok
        # combos(["m"],[500],[10],[24],["slog"])
        # combos(["m"],[700],[10],[24],["slog"])
        #combos(["m"],[800],[10],[24,48,96,3,6,12],["slog"])
        
        # combos(["m"],[1200],[10],[24,48],["slog"])
        #combos(["m"],[500],[10],[96,3,6,12],["slog"])

        combos(["m"],[390,395,400,410,420,430,440,450],[12],[24,48],["slog"])

        #combos(["k"],[10],[3],[50],["slog"])
        #combos(["k"],[10],[3],[47],["slog"])
        #combos(["k"],[10],[3],[49],["slog"])
        #combos(["k"],[6,7,8],[3,4],[3, 6, 12, 24, 48, 96],["both"])
        #combos(["k"],[10,11,12,13],[3],[3, 6, 12, 24, 48, 96],["slog"])
