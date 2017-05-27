#!/bin/bash
#SBATCH --ntasks=100
#SBATCH --exclude=cn[01-324]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=<mohd.hasan@uconn.edu>
#SBATCH --partition=general

module load r/3.3.3
Rscript ./scratch_with_chunks.R