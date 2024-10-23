#!/bin/bash

#SBATCH --mem=247g
#SBATCH --cpus-per-task=40
#SBATCH -o test_volCov_trends_significance.out
#SBATCH -e test_volCov_trends_significance.err
#SBATCH --partition=unlimited

module load R/4.2.1
Rscript /data/phamlk/combined_cov_project/test_volCov_trends_significance.R 
