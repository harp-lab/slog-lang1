"""
Benchmark Harness
"""

import logging
import os
import shutil
import tempfile
from typing import Iterator


# class ExecutionResult:
#     """ result class for each datalog run """

#     def __init__(self, engine_name, dataset_name, cores,
#                  runtime, memory_usage) -> None:
#         self.engine_name = engine_name
#         self.dataset_name = dataset_name
#         self.cores = cores
#         self.runtime = runtime
#         self.memory_usage = memory_usage


class Dataset:
    """ dataset class, all file inside dataset folder must be either csv/tsv/facts/ """

    def __init__(self, name: str, data_dir: str, row_sep: str) -> None:
        """ type is csv/tsv/facts """
        self.name = name
        self.data_dir = data_dir
        self.row_sep = row_sep
        self.files = os.listdir(data_dir)

    def fetch_data(self, rel_fname) -> Iterator[list]:
        """
        return a tuple iterator of each row, tuple in dataset is processed as python list
        """
        rel_file_path = os.path.join(self.data_dir, rel_fname)
        if os.path.exists(rel_file_path):
            with open(rel_file_path) as rel_f:
                for row in rel_f:
                    cols = row.split(self.row_sep)
                    if cols != []:
                        yield list(map(lambda x: x.strip(), cols))
        else:
            logging.error("Relation %s not exists in dataset %s",
                          rel_fname, self.name)
            return []

    def dump(self, out_dir, fname_mapping, data_format="tsv", customize_format_function=None):
        """
        dump a dataset to target path
        """
        if os.path.exists(out_dir):
            shutil.rmtree(out_dir)
        os.mkdir(out_dir)
        if (self.row_sep == '\t' and data_format in ['tsv', 'facts']) and \
                (self.row_sep == ',' and data_format in ['csv']):
            for fname in self.files:
                shutil.copyfile(os.path.join(self.data_dir, fname),
                                os.path.join(out_dir, fname_mapping[fname]))
        else:
            for fname in self.files:
                with open(fname_mapping[fname], "w+") as out_f:
                    for row in self.fetch_data(fname):
                        new_row_txt = ""
                        if data_format == 'tsv':
                            new_row_txt = "\t".join(row)
                        elif data_format in ['csv', 'facts']:
                            new_row_txt = ",".join(row)
                        else:
                            new_row_txt = customize_format_function(row)
                        out_f.write(new_row_txt)


class DatalogEngine:
    """ datalog engine abstract class """

    def __init__(self, name, verbose=False) -> None:
        self.name = name
        self.verbose = verbose

    def run(self, dataset: Dataset, output_file, src, file_mapping, cores):
        """
        data_input: dataset
        output: statistic info output path
        core: core counts used to run benchmark
        file_mapping: mapping from dataset file to datalog input facts file
        """


class BenchmarkCase:
    """ one time test """

    def __init__(self, datalog: DatalogEngine, dataset: Dataset, src, file_mapping, cores) -> None:
        self.datalog = datalog
        self.dataset = dataset
        self.file_mapping = file_mapping
        self.cores = cores
        self.datalog_file = src

    def run(self, output_file):
        self.datalog.run(self.dataset, output_file, self.datalog_file,
                         self.file_mapping, self.cores)

    def __str__(self) -> str:
        prog_name = os.path.basename(self.datalog_file)
        return f"{self.datalog.name}_{self.dataset.name}_{prog_name}_{self.cores}"


class Slog(DatalogEngine):
    """ slog test harness """

    def __init__(self, verbose=False) -> None:
        super().__init__("slog", verbose)

    def run(self, dataset: Dataset, output_file, src, file_mapping, cores):
        program_name = os.path.basename(src)[:-5]
        with tempfile.TemporaryDirectory() as tempdir_name:
            dataset.dump(tempdir_name+'/in', file_mapping, 'csv')
            logging.info(
                "Running slog %d cores, dataset %s ..., file %s", cores, dataset.data_dir, src)
            os.system(
                f"cd /slog && ./runslog -v -co -j {cores} -f {tempdir_name}/in {src} {tempdir_name}/out")
            os.system(
                f"cd {tempdir_name}/out/build && /usr/bin/time -v -o {output_file} ./{program_name} -j {cores}")


class Souffle(DatalogEngine):
    """ souffle test harness """

    def __init__(self, verbose=False) -> None:
        super().__init__("souffle", verbose)

    def run(self, dataset: Dataset, output_file, src, file_mapping, cores):
        program_name = os.path.basename(src)[:-3]
        with tempfile.TemporaryDirectory() as tempdir_name:
            dataset.dump(tempdir_name+'/in', file_mapping, 'csv')
            out_dir = os.path.join(tempdir_name, "out")
            os.mkdir(out_dir)
            logging.info(
                "Running souffle %d cores, dataset %s ..., file %s", cores, dataset.data_dir, src)
            os.system(
                f"souffle -o {tempdir_name}/{program_name} -j {cores} {src}")
            os.system(
                f"/usr/bin/time -v -o {output_file} {tempdir_name}/{program_name} -j {cores} -F {tempdir_name}/in -D {out_dir}")


class Benchmark:
    """ benchmark entrance class """

    def __init__(self, case_list, output_dir) -> None:
        self.case_list = case_list
        self.output_dir = output_dir

    def run(self):
        """ start benchmark """
        for bench_case in self.case_list:
            output_fpath = os.path.join(self.output_dir, str(bench_case))
            bench_case.run(output_fpath)


if __name__ == "__main__":
    """ test code """
    test_dataset = Dataset("test", "/slog/test/testcase/tc/edge", "\t")
    souffle_engine = Souffle()
    slog_engine = Slog()
    target_slog_program = "/slog/test/testcase/tc/tc.slog"
    target_souffle_program = "/slog/test/testcase/tc/tc.slog"
    case_list = []
    for i in [1, 3, 6]:
        case_list.append(BenchmarkCase(
            slog_engine, test_dataset, target_slog_program,
            {
                "edge.facts": "edge.facts"
            },
            i
        ))
        case_list.append(BenchmarkCase(
            souffle_engine, test_dataset, target_souffle_program,
            {
                "edge.facts": "edge.facts"
            },
            i
        ))
    bench_out = "/benchmark_out"
    Benchmark(case_list, bench_out).run()
    print(f"Benchmark finished, result in {bench_out}.")
