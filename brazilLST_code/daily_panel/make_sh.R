# write .sh scripts needed to generate daily panel in the cluster

setwd('E:/brazil_lst/code/daily_panel')

ylist <- c('03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

for (y in ylist){

filename <- file(paste("lst",y,".sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH lst_dpanel_",y,"_CL.R", sep=""), filename, append=TRUE)
close(filename)

}

### LST panel merge .sh script

filename <- file(paste("lst_m.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH lstpanel_all_CL.R", sep=""), filename, append=TRUE)
close(filename)

### Make daily panel

### LST panel merge .sh script

filename <- file(paste("dpanel.sh", sep=""), open="w")
write('#!/bin/bash', filename, append = TRUE)
write('#SBATCH --partition=batch', filename, append = TRUE)
write('#SBATCH -c 8', filename, append = TRUE)
write('#SBATCH --mem=32g', filename, append = TRUE)
write('#SBATCH --nodes=1', filename, append = TRUE)
write('#SBATCH --time=48:00:00', filename, append = TRUE)
write('module load R/3.2.2', filename, append = TRUE)
write(paste("R CMD BATCH dpanel_all_CL.R", sep=""), filename, append=TRUE)
close(filename)