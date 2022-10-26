#-----------------------------------------------------------------------------#
# Title: Import USGS StreamGauge data
# Author: B Saul
# Date: 11/11/15
# Purpose: Import flow & temp data from USGS  for
#
# 03501975  LITTLE TENNESSEE RIVER AB NC HWY 28 AT IOTLA, NC
# 03503000	LITTLE TENNESSEE RIVER AT NEEDMORE, NC
#
# 03510577	TUCKASEGEE RIVER AT BARKER'S CREEK, NC
#-----------------------------------------------------------------------------#

library(dplyr)
sites <- c('03501975', '03503000', '03510577')

usgsdt <- lapply(sites, FUN = function(x){
  dataRetrieval::readNWISdv(x,
                            parameterCd = c('00010', '00060'), # temp & discharge
                            startDate   = '2012-01-01',
                            endDate     = '2014-12-31',
                            statCd      = '00003' #Daily mean
                            ) }) %>%
  bind_rows() %>%
  mutate(river = ifelse(site_no == '03510577', 'Tuck', 'LiTN'),
         date  = as.Date(Date)) %>%
  select(river, site_no, date, temp = X_00010_00003, discharge = X_00060_00003)

siteInfo <- dataRetrieval::readNWISsite(sites)

save(usgsdt, file = 'data/usgsdt.rda')
