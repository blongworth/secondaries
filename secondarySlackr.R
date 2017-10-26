# Secondary Slackr
# Writes a summary table of secondary performance for the last three
# months to Slack.
# Slack params in ~/.slackr

#Load Libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(amstools)
library(slackr)

#set parameters
from <- Sys.Date() - 90
to <- 'present' #present or date
sys <- 'both' #cfams, usams, ams1 or both
size <- c(40,300)

#Set digits for output
options(digits=4)

# Get fresh secondary data
#out <- getQCData(from, to, sys)

#Load QC data from file
load("qcData.rda")

# Setup slack
slackr_setup()

#Filter data
out.s <- filter(out, primary == FALSE, # rec_num != 113385, #don't use normalizing ox-I
                f_modern > .1, f_modern < 1.6,
                tp_date_pressed >= as.Date(from),
                tp_date_pressed <= ifelse(to != 'present', to, Sys.Date()),
                gf_co2_qty > size[1], gf_co2_qty < size[2], #Select size range
                lab == "OSG", #Use only samples from the SPL
                is.na(q_flag), #Check for q_flag
                abs(sigma) < 5, #Select reasonable sigmas
                abs(normFm) < 0.03, #Select reasonable Fm
                frep_err < 0.10
              )

#Make summary table
syssum <- out.s %>%
  group_by(system) %>%
  summarize(
    nFm = mean(normFm),
    nFm.s = sd(normFm),
    sig = mean(sigma),
    sig.sd = sd(sigma),
    ferr = mean(frep_err),
    N = n()
    )

#knitr::kable(syssum, digits = 5, caption = "Summary data for secondaries")
#slackr(str(iris))
slackr("Summary of secondaries", from, "to present\n", print(syssum))
#print(table, floating=TRUE, type="html")
