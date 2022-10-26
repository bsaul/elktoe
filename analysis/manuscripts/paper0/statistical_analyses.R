#-----------------------------------------------------------------------------#
# Title: Elktoe Statistical Analyses
# Author: B Saul
# Date: 1/15/15
# Purpose:
#-----------------------------------------------------------------------------#

library(elktoe)

musseldt <- mussels_wide %>%
  # Exclude L. fasciola removed in 2014
  filter(!(species == 'L. fasciola' & date_1 > '2014-01-01') ) %>%

  mutate(final_status = ifelse(is.na(dead_1), 'M', dead_1) ,
         weight_1     = ifelse(species == 'A. raveneliana',
                               buoyant_weight_g_1,
                               dry_weight_g_1),
         weight_0     = ifelse(species == 'A. raveneliana',
                               buoyant_weight_g_0,
                               dry_weight_g_0),
         weight_ratio = weight_1/weight_0,

         lost25      = (weight_ratio < .75) * 1,
         deadorlost25 = ifelse(lost25 == 1 | final_status == '1', 1, 0) )%>%

  filter(final_status != 'M')


analysisdt <- musseldt %>%
  group_by(species, river) %>%
  summarise(dead = sum(dead_1),
            deadorlost25 = sum(deadorlost25),
            n    = n()) %>%
  filter(species == 'A. raveneliana')

#### Test of Proportions ###
prop.test(analysisdt$dead, analysisdt$n)
prop.test(analysisdt$deadorlost25, analysisdt$n)


#### Trend Test

sitedt <- musseldt %>%
  group_by(species, river, site) %>%
  summarise(dead = sum(dead_1),
            deadorlost10 = sum(deadorlost10),
            n    = n())




litn <- sitedt %>%
  filter(species == 'A. raveneliana') %>%
  filter(river == 'Little Tennessee')

tuck <- sitedt %>%
  filter(species == 'A. raveneliana') %>%
  filter(river == 'Tuckasegee')

prop.trend.test(tuck$dead, tuck$n)
