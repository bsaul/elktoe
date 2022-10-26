#------------------------------------------------------------------------------#
#   TITLE: Import metabolomics data
#    DATE: 2017DEC18
#  AUTHOR: Bradley Saul
# PURPOSE: Import the positive and negative ion data files available from
#          Scott S described as raw data. These do not appear to be raw data
#          as in off the machine. The data appeared to be preprocessed by binning
#          and normalization.
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(stringr)

data_file_neg <- "inst/metabolomics/RAW DATA/Export-NEG-04102015.csv"
data_file_pos <- "inst/metabolomics/RAW DATA/Export-POS-04102015.csv"

### functions ####
create_long_data <- function(input_data, colremove, keepchar){
  names(input_data) <- tolower(names(input_data))
  input_data %>%
    # Get rid of variables that can be computed later
    select(-anova..p., -highest.mean, -lowest.mean, -maximum.abundance,
           # Drop 'tags' columns and anything to the right - not sure what these are
           -minimum.cv., -max.fold.change, -c(colremove:ncol(input_data))) %>%
    select(compound, neutral_mass_da = neutral.mass..da., mz = m.z,
           charge, retention_time_min = retention.time..min.,
           chroma_peak_width_min = chromatographic.peak.width..min., identifications,
           isotope_distribution = isotope.distribution, contains(keepchar)) %>%
    gather(key = key, value = norm_abundance, -compound, -neutral_mass_da, -mz, -charge,
           -retention_time_min, -chroma_peak_width_min, -identifications, -isotope_distribution) %>%
    mutate(
      mussel_id      = toupper(str_extract(key, "[[:lower:]][[:digit:]]{3}")),
      tissue_type    = str_extract(key, "gill"),
      unknown_class  = str_extract(key, ".{1}$")
    )
}

### Read and clean NEG ####
raw_neg  <- read.csv(file = data_file_neg, skip = 2)
metab_neg <- create_long_data(raw_neg, 47, "neg") %>%
  select(-key)

### Read and clean POS ####
raw_pos  <- read.csv(file = data_file_pos, skip = 2)
metab_pos <- create_long_data(raw_pos, 277, "pos") %>%
  mutate(
    mussel_id = if_else(is.na(mussel_id), str_extract(key, "pool.{3}"), mussel_id),
    mussel_id = if_else(is.na(mussel_id), str_extract(key, "(tr|mh)_[[:digit:]]+"), mussel_id)
  ) %>%
  select(-key)


### Save files ####

save(metab_neg, file = "data/metab_neg.rda")
save(metab_pos, file = "data/metab_pos.rda")
devtools::install()
