#-----------------------------------------------------------------------------#
# Title: Import feeding trial data
# Author: B Saul
# Date: 3/9/15
# Purpose: Load and save data objects for analysis ####
#-----------------------------------------------------------------------------#

dir   <- c('corbicula', 'lampsilis', 'elktoe')
times <- c(seq(0, 3, by = .5), 4)
reps  <- c(1:10, paste0('c', 1:3))
files <- expand.grid( time = times, rep_no = reps, dir = dir)
files <- within(files,{
                file_name <- paste0(dir, '/DIA-t', time, ' ', rep_no)
                id        <- ifelse(grepl('^c', rep_no),
                                    paste0(substr(dir, 1, 4), '_', rep_no),
                                    paste0(substr(dir, 1, 4), rep_no))
                genus_trial <- as.character(dir)
                genus       <- ifelse(grepl('_', id), 'control', as.character(dir))
                tank_c      <- ifelse(genus == 'control',
                                      paste(genus_trial, 'control'), genus)
})

# cannot open file 'data/raw_data/feeding_trial/lampsilis/DIA-t0.5 c3.txt':
# No such file or directory
## Removing this file

files <- files[!files$file_name == 'lampsilis/DIA-t0.5 c3', ]

## Extra text in following files causing errors
 # lampsilis/DIA-t2.5 1
 # Removed offending text and since it was a single column assumed this was
 # 'Measured Diameters' like the other files

files[files$file_name == 'lampsilis/DIA-t2.5 1', 'file_name'] <- 'lampsilis/DIA-t2.5 1_2'

 # corbicula/DIA-t0.5 1
 # same issue and resolution as lampsilis/DIA-t2.5 1
files[files$file_name == 'corbicula/DIA-t0.5 1', 'file_name'] <- 'corbicula/DIA-t0.5 1_2'

 # corbicula/DIA-t1 1
 # This is not a single column of text. Excluding pending further investigation.
 # files <- files[!files$file_name == 'corbicula/DIA-t1 1', ]
 # confirmed with S Salger 3/12 to just use first column of values
files[files$file_name == 'corbicula/DIA-t1 1', 'file_name'] <- 'corbicula/DIA-t1 1_2'

#### Import data ####

trial <- by(files, files$file_name, FUN = function(x) {
  file <- paste0('data/raw_data/feeding_trial/', x$file_name, '.txt')
  vals <- read.table(file = file, skip = 1)
  dt   <- data.frame(diameter = vals,
                     id       = x$id,
                     time     = x$time,
                     rep_no   = x$rep_no,
                     genus_trial = x$genus_trial,
                     genus    = x$genus,
                     tank_c   = x$tank_c)
  return(dt)
})

trial <- plyr::rbind.fill(trial)
names(trial)[1] <- 'diameter'

#### SAVE DATA ####
save(trial, file = 'data/trial.rda')

## spot check
#trial[trial$id == 'elkt_c1', ]
#trial[trial$id == 'lamp8', ]
