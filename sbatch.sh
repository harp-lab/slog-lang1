#!/bin/bash
#SBATCH --nodes=2
#SBATCH --ntasks=256
#SBATCH --ntasks-per-node=128
#SBATCH --cpus-per-task=1 
srun /home/ubuntu/slog/backend/tests/cc/compiled_pre/build/cc /home/ubuntu/dataset/twitter /home/ubuntu/srun-out
