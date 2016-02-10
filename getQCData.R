#!/usr/bin/r
#getQCData
#R script to get QC data from database and write it to a file

#Load Libraries
library(amstools)

#set parameters
from <- '2014-09-01'
to <- 'present' #present or date
sys <- 'both' #cfams, usams, ams1 or both

#Get the data
out <- getQCData(from, to, sys)

#write to file
save(out, file = "qcData.rda")
