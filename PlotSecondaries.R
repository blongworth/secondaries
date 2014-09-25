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
nosams <- odbcConnect("nosams-prod", uid = "brettl", pwd = "8675309")

#get intcal table
intcal <- sqlFetch(nosams, "intercal_samples")

#Close DB
odbcClose(nosams)

#create factor of tiri_id, order by Fm
intcal <- within(intcal, name <- factor(tiri_id, levels = unique(tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

#Replace C-6 with new consensus from Xiaomei 2010
intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

#add in OX-I, OX-II
#ox <- read.csv("intcalox.csv")
#intcal <- rbind(intcal, ox)

#and/or combine duplicates?

#Create data frame of all secondaries with sigmas
out <- lapply(X = as.list(intcal$rec_num), FUN = calcSecondary, from, to, system)
out <- do.call("rbind", out)

#clean it up
out$system[grepl("CFAMS", out$wheel_id)] <- "CFAMS"
out$system[grepl("USAMS", out$wheel_id)] <- "USAMS"
out$system[is.na(out$system)] <- "AMS1"
out <- merge (out,intcal[, c("rec_num", "name")], by= "rec_num")

#Subset data later (more visible)
#out.c <- subset(out, sigma < 10 & sigma > -10)
# out.c <- subset(out.c, normFm < .05 & normFm > -.05)
# out.c <- subset(out.c, f_modern > .1 & f_modern < 1.6)
# out.s <- out #subset(out.c, gf_co2_qty < 150 & gf_co2_qty > 40)

#summary table
# secsum <- ddply(out, .(tiri_id, system), summarize,
#                 fm = mean(f_modern),
#                 fm.s = sd(f_modern),
#                 nFm = mean(normFm),
#                 nFm.s = sd(normFm),
#                 sig = mean(sigma),
#                 sig.s = sd(sigma),
#                 N = length(f_modern))

# ####
# #Results
# ####
# 
# #mean sigma
# mean(out.s$sigma)
# 
# #Sigma histogram
# p <- qplot(sigma, data=out.s, geom="histogram", xlim=c(-10,10))
# p + ggtitle(paste("Sigma Histogram, ", system, ", ", from))
# 
# #sigma boxplots
# 
# p <- qplot(name, sigma, data=out.m, geom=c("boxplot", "jitter"))
# p + ggtitle(paste("Sigma Boxplots, ", system, ", ", from)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# 
# #plot size vs sigma
# p <- qplot(gf_co2_qty, sigma, data=out.s)
# p + geom_smooth(method="lm") + ggtitle(paste("Sigma vs umol CO2, ", system, ", ", from))
# 
# p <- qplot(gf_co2_qty, normFm, data=out.s)
# p + geom_smooth(method="lm") + ggtitle(paste("Normfm vs umol CO2, ", system, ", ", from))
# 
# #ratio vs sigma
# p <- qplot(f_modern, sigma, color=gf_co2_qty, data=out.s)
# p + geom_smooth(method="lm") + ggtitle(paste("Sigma vs Fm, ", system, ", ", from))
# 
# p <- qplot(fm, sig, data=secsum)
# p + geom_smooth(method="lm") + geom_errorbar(aes(ymin=sig-sig.s, ymax=sig+sig.s), width=.1) +
#   geom_point(size=4) + ggtitle(paste("Sigma vs Fm, ", system, ", ", from))
# 
# #linear regression sigma
# summary(lm(sigma~f_modern, data=out.s))
# 
# #norm ratio vs ratio
# qplot(f_modern, normFm, data=out.s) -> p
# p + geom_smooth(method="lm") + ggtitle(paste("NormFm vs Fm, ", system, ", ", from))
# 
# #sigma vs date
# p <- qplot(tp_date_pressed, sigma, data=out.s)
# p + geom_smooth(method="lm") + ggtitle(paste("Sigma vs date pressed, ", system, ", ", from))
