#!/usr/bin/r
#getQCData
#R script to get QC data from database and write it to a file

#Load Libraries
library(amstools)

#set parameters
from <- '2014-09-01'
to <- 'present' #present or date
sys <- 'both' #cfams, usams, ams1 or both

#Get intcal data
out <- getQCData(from, to, sys, getcurrents = TRUE)

#Get Standards table data
std <- getQCData(from, to, sys, intcal = FALSE, getcurrents = TRUE)

#Get qc data
qc <- getQCData(from, to, sys, useQC = TRUE)

#write to file
save(out, std, qc,  file = "qcData.rda")
