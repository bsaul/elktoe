#-----------------------------------------------------------------------------#
# Title: Import Mussels data
# Author: B Saul
# Date: 8/14/14
# Purpose: Load and save data objects for analysis####
#-----------------------------------------------------------------------------#

#setwd('/Users/bradley/Dropbox/Research/mussels')
library(plyr)
library(reshape2)
library(lubridate)

#-----------------------------------------------------------------------------#
# Corbicula ####
#-----------------------------------------------------------------------------#

corbicula <- read.csv(file = 'data/raw_data/corbicula.csv', header = T)
corbicula$corb_density <- apply(corbicula[, -(1:2)], 1, mean, na.rm = T)

#-----------------------------------------------------------------------------#
# Mussels ####
#-----------------------------------------------------------------------------#

#### Load Mussel Data ####

mussels <- read.csv(file = 'data/raw_data/mussel_metrics.csv', header = T,
                    stringsAsFactors = F)
mussels <- mussels[order(mussels$species), ]

center_scale_by_species <- function(var){
  as.numeric(unlist(tapply(var, mussels$species, scale)))
}

mussels <- within(mussels, {
  date          <- as.Date(date, format = "%m/%d/%y")
  volume        <- length_mm * width_mm * height_mm
  s_length      <- center_scale_by_species(length_mm)
  s_height      <- center_scale_by_species(height_mm)
  s_width       <- center_scale_by_species(width_mm)
  s_volume      <- center_scale_by_species(volume)
  s_buoyant     <- center_scale_by_species(buoyant_weight_g)
  s_dry         <- center_scale_by_species(dry_weight_g)
})

# merge corbicula density into mussels
mussels <- merge(mussels, corbicula[, c('site', 'corb_density')], by = 'site')


mussels_wide <- reshape(mussels, timevar = 'post',
                        idvar = c('id', 'species', 'river', 'site'),
                        direction = 'wide', sep = '_')
mussels_wide <- within(mussels_wide, {
  dead               = dead_1
  length_mm_d        = length_mm_1 - length_mm_0
  height_mm_d        = height_mm_1 - height_mm_0
  width_mm_d         = width_mm_1 - width_mm_0
  volume_d           = volume_1 - volume_0
  dry_weight_g_d     = dry_weight_g_1 - dry_weight_g_0
  s_dry_d            = s_dry_1 - s_dry_0
  buoyant_weight_g_d = buoyant_weight_g_1 - buoyant_weight_g_0
  s_buoyant_d        = s_buoyant_1 - s_buoyant_0
  p_buoyant_weight_d = buoyant_weight_g_1/buoyant_weight_g_0
  days               = date_1 - date_0
  lost_wt_or_dead    = ifelse(dead_1 == 1 | buoyant_weight_g_d < 1, 1, 0)
  lost_10_or_dead    = ifelse(dead_1 == 1 | p_buoyant_weight_d < .9, 1, 0)
})

#-----------------------------------------------------------------------------#
# River ####
#-----------------------------------------------------------------------------#
river <- read.csv(file = 'data/raw_data/river_metrics.csv', header = T,
                  stringsAsFactors = F,
                  col.names = c('site', 'river', 'date', 'do', 'turb', 'ph', 'temp'))

river$date_ <-as.Date(mdy(river$date))
river$siteno <- substr(river$site, 6,6)

#-----------------------------------------------------------------------------#
# Button Logger ####
#-----------------------------------------------------------------------------#
button <- read.csv(file = 'data/raw_data/button_temp.csv', header = T,
                  stringsAsFactors = F,
                  col.names = c('site', 'date_time', 'temp', 'qc_flag')) %>%
  mutate(qc_flag = ifelse(is.na(button$qc_flag), 0, button$qc_flag),
         river = ifelse(grepl('LiTN', button$site), 'Little Tennessee', 'Tuckasegee'),
         date_time = mdy_hm(date_time),
         siteno = substr(button$site, 6,6)) %>%
  arrange(date_time)



#-----------------------------------------------------------------------------#
# Save objects ####
#-----------------------------------------------------------------------------#
save(mussels, file = 'data/mussels.rda')
save(corbicula, file = 'data/corbicula.rda')
save(mussels_wide, file = 'data/mussels_wide.rda')
save(river, file = 'data/river.rda')
save(button, file = 'data/button.rda')
#save(mussels, mussels_wide, river, file = 'mussels.RData')

