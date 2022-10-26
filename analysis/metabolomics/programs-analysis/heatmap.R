#------------------------------------------------------------------------------#
#   TITLE: creates a heatmap of metabolomic data
#    DATE: 2017DEC22
#  AUTHOR: Bradley Saul
# PURPOSE:
#------------------------------------------------------------------------------#

library(elktoe)
library(tidyr)


temp <- metab_pos %>%
  filter(grepl("^(P|C)", mussel_id)) %>%
  # filter(grepl("^C", mussel_id)) %>%
  # filter(mussel_id != "P114") %>%
  spread(key = mussel_id, value = norm_abundance)

heatmap(log(t(temp[, 11:ncol(temp)]) + 1))
