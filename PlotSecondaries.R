#Plot secondary sigma results

require(ggplot2)
require(RODBC)
require(plyr)

if (!exists("from")) {
  #set parameters
  from <- '2012-01-01'
  to <- 'present' #present or date
  system <- 'cfams' #cfams or ams1
  
}

#Run CalcSecondaries to get Functions
source("CalculateSecondaries.R")


####
#Do the stuff
####

#Open DB connection
nosams <- odbcConnect(database, uid = uid, pwd = pwd)

#get intcal table
intcal <- sqlFetch(nosams, "intercal_samples")

#Close DB
odbcClose(nosams)

#create factor of tiri_id, order by Fm
intcal <- within(intcal, name <- factor(tiri_id, levels = unique(tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

#Replace C-6 with new consensus from Xiaomei 2010
intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

#add in OX-I, OX-II
ox <- read.csv("intcalox.csv")
intcal <- rbind(intcal, ox)

#and/or combine duplicates?

#Create data frame of all secondaries with sigmas
out <- lapply(X = as.list(intcal$rec_num), FUN = calcSecondary, from, to, system)
out <- do.call("rbind", out)


#clean it up
out$system[grepl("CFAMS", out$wheel_id)] <- "CFAMS"
out$system[grepl("USAMS", out$wheel_id)] <- "USAMS"
out$system[is.na(out$system)] <- "AMS1"
out <- merge (out,intcal[, c("rec_num", "name")], by= "rec_num")

