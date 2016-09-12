# write .sh scripts needed to generate daily panel in the cluster

setwd('E:/brazil_lst/code/station_panel')

ylist <- c('03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

for (y in ylist){
  
  filename <- file(paste("st_lst_",y,".sh", sep=""), open="w")
  write('#!/bin/bash', filename, append = TRUE)
  write('#SBATCH --partition=batch', filename, append = TRUE)
  write('#SBATCH -c 8', filename, append = TRUE)
  write('#SBATCH --mem=32g', filename, append = TRUE)
  write('#SBATCH --nodes=1', filename, append = TRUE)
  write('#SBATCH --time=48:00:00', filename, append = TRUE)
  write('module load R/3.2.2', filename, append = TRUE)
  write(paste("R CMD BATCH st_lst_dpanel_",y,"_CL.R", sep=""), filename, append=TRUE)
  close(filename)
  
}

### BEST 

filename <- file(paste("st_best.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH st_best_CL.R", sep=""), filename, append=TRUE)
close(filename)

### PPT

filename <- file(paste("st_ppt.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH st_ppt_CL.R", sep=""), filename, append=TRUE)
close(filename)

### NFA

filename <- file(paste("st_nfa.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH st_nfa_CL.R", sep=""), filename, append=TRUE)
close(filename)

### LST MERGE

filename <- file(paste("st_lstpanel.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH st_lstpanel_CL.R", sep=""), filename, append=TRUE)
close(filename)

### MERGE

filename <- file(paste("st_merge.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH st_merge_CL.R", sep=""), filename, append=TRUE)
close(filename)