source("http://bioconductor.org/biocLite.R")
biocLite("xcms")
library(xcms)
library(elktoe)

cdfs <- dir(system.file("cdf", package = "faahKO"), full.names = TRUE,
            recursive = TRUE)


inFile <- "analysis/metabolomics/ST000495_AN000761.mwTab"

dt <- readMSData(files = inFile)
