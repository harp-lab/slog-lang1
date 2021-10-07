# python3 csvs_to_bins.py csv_dir slog_dir
import os
import shutil
import subprocess
import glob

TSV2BIN_PATH = os.path.join(os.path.dirname(__file__), "../../backend/build/tsv_to_bin")

if ARGC != 2:
    print("csv_to_bins.py <csv_dir> <output_slog_db>")
    print("Stages all .fact files in <csv_dir> into <output_slog_db>")
    exit(1)

fact_files = glob.glob("{}/*.facts")

for file in fact_files:
    print(file)
