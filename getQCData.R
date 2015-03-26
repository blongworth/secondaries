#getQCData
#R script to get QC data from database and write it to a file

#Load Libraries
library(plyr)
library(dplyr)
library(RODBC)

#Load files
source("~/R/dbconfig.R") #DB connection info
source("CalculateSecondaries.R") #Functions

#set parameters
from <- '2014-09-01'
to <- 'present' #present or date
sys <- 'both' #cfams, usams, ams1 or both

#Get the data
out <- getQCData(from, to, sys)

#write to file
write.csv(out, "qcData.csv")
