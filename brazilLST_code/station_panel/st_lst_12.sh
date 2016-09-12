#!/bin/bash
#SBATCH --partition=batch
#SBATCH -c 8
#SBATCH --mem=32g
#SBATCH --nodes=1
#SBATCH --time=48:00:00
module load R/3.2.2
R CMD BATCH st_lst_dpanel_12_CL.R
