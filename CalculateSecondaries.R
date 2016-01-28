#Functions for secondary standard analysis

source("~/R/dbconfig.R") #DB connection info

####
#Define functions
####

se <- function(x) {
  #Calculate standard error
  sqrt(var(x)/length(x))
}

sigma <- function(fmm, fmc, em, ec = 0) {
  #Calculate Sigma
  (fmm - fmc) / sqrt(em^2 + ec^2)
}

normFm <- function (fmm, fmc) {
  #Calculate normalized fm
  (fmm - fmc) / fmc
}

intrErr <- function(totErr, targErr) {
  #calculate intrinsic error
  ifelse (totErr < targErr, NA, sqrt(totErr^2 - targErr^2))
}

totErr <- function(targErr, intrErr) {
#Calculate total error
  sqrt(targErr^2 + intrErr^2)
}

getSample <- function (rec, from, to = "present", sys = "both") {
  #Get data for a rec_num from database
  #Return query result as data table

  #What system do we want data for?
  if (sys == "cfams") {
    whid <- "AND wheel_id LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel_id LIKE 'U%'"
  } else if (sys =="both") {
    whid <- "" 
  } else {
    whid <- "AND wheel_id NOT LIKE 'C%'"
  }

  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }
  
  db <- RODBC::odbcConnect(database, uid = uid, pwd = pwd)
  
  
  query <- paste("
                 SELECT target.rec_num, target.tp_num, target.osg_num,
                   target.target_name, wheel_pos.wheel_id, 
                   target.tp_date_pressed, graphite_lab.lab_name, 
                   no_os.f_modern, no_os.f_int_error, no_os.f_ext_error, 
                   no_os.dc13, graphite.gf_co2_qty, no_os.q_flag, 
                   snics_results.sample_type, snics_results.sample_type_1
                 FROM no_os, target, wheel_pos, graphite, graphite_lab, 
                   snics_results
                 WHERE target.tp_num = no_os.tp_num 
                   AND target.tp_num = wheel_pos.tp_num 
                   AND target.tp_num = snics_results.tp_num
                   AND target.osg_num = graphite.osg_num 
                   AND target.graphite_lab = graphite_lab.lab_id
                   AND target.rec_num =", rec," 
                   ",whid, "
                   AND target.tp_date_pressed > '",from,"'
                   ", ts, "
                 ")

  #Do the queries
  f <- RODBC::sqlQuery(db, fquery)
  cur <- sqlQuery(db, paste("
                            SELECT snics_raw.tp_num, AVG(le12c) AS le12c
                            FROM snics_raw, target
                            WHERE target.tp_num = snics_raw.tp_num
                            AND ok_calc = 1
                            AND target.rec_num =", rec," 
                            ",whid, "
                            AND target.tp_date_pressed > '",from,"'
                            ", ts, "
                            GROUP BY snics_raw.tp_num
                            "))

  count <- sqlQuery(db, paste("
                            SELECT snics_raw.tp_num, SUM(cnt_14c) AS counts
                            FROM snics_raw, target
                            WHERE target.tp_num = snics_raw.tp_num
                            AND ok_calc = 1
                            AND target.rec_num =", rec," 
                            ",whid, "
                            AND target.tp_date_pressed > '",from,"'
                            ", ts, "
                            GROUP BY snics_raw.tp_num
                            "))

  RODBC::odbcClose(db)
  
  left_join(f, count, by = "tp_num")
}

getStandards <- function (from, to = "present", sys = "both") { 
  #Get data for all secondaries from database
  #Return query result as data table
  
  #What system do we want data for?
  if (sys == "cfams") {
    whid <- "AND wheel_id LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel_id LIKE 'U%'"
  } else if (sys =="both") {
    whid <- "" 
  } else {
    whid <- "AND wheel_id NOT LIKE 'C%'"
  }
  
  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }
  
  db <- RODBC::odbcConnect(database, uid = uid, pwd = pwd)
  

  dquery <- paste("
              SELECT 
                target.rec_num, target.tp_num, target.osg_num,
                target.target_name, wheel_pos.wheel_id, 
                target.tp_date_pressed, graphite_lab.lab_name, 
                no_os.f_modern, no_os.f_int_error, no_os.f_ext_error, 
                no_os.dc13, graphite.gf_co2_qty, no_os.q_flag, 
                snics_results.sample_type, snics_results.sample_type_1
              FROM target
                INNER JOIN no_os
                  ON target.tp_num = no_os.tp_num
                INNER JOIN wheel_pos
                  ON target.tp_num = wheel_pos.tp_num
                INNER JOIN snics_results
                  ON target.tp_num = snics_results.tp_num
                INNER JOIN graphite
                  ON target.osg_num = graphite.osg_num
                INNER JOIN graphite_lab
                  ON target.graphite_lab = graphite_lab.lab_id
                INNER JOIN standards
                  ON target.rec_num = standards.rec_num
              WHERE 
                target.tp_date_pressed > '",from,"'
                ", ts, "
                ",whid, "
              ")

  cquery <- paste("
              SELECT 
                snics_raw.tp_num, 
                AVG(le12c) AS le12c,
                SUM(cnt_14c) AS counts
              FROM snics_raw
                INNER JOIN target
                  ON snics_raw.tp_num = target.tp_num
                INNER JOIN standards
                  ON target.rec_num = standards.rec_num
              WHERE
                ok_calc = 1
                ",whid, "
                AND target.tp_date_pressed > '",from,"'
                ", ts, "
              GROUP BY snics_raw.tp_num
              ")


  #Do the queries
  data <- RODBC::sqlQuery(db, dquery)
  cur <- RODBC::sqlQuery(db, cquery)

  RODBC::odbcClose(db)

  dplyr::left_join(data, cur, by = "tp_num")
  
}

getIntcalTable <- function() {
  
  #Open DB connection
  db <- odbcConnect(database, uid = uid, pwd = pwd)
  
  #get intcal table
  intcal <- sqlQuery(db, paste("select * from ", "intercal_samples"))
  
  RODBC::odbcClose(db)

  #create factor of tiri_id, order by Fm
  intcal <- within(intcal, name <- factor(tiri_id, levels = unique(
                   tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))

  #Replace C-6 with new consensus from Xiaomei 2010
  intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016

  #add in OX-I, OX-II
  ox <- read.csv("intcalox.csv")
  intcal <- rbind(intcal, ox)

  #add process type
  ps <- read.csv("intcal_process.csv")
  ps <- ps %>% select(rec_num, process)
  intcal <- inner_join(intcal, ps) 
  return(intcal)
}

getStdTable <- function() {
  
  #Open DB connection
  db <- odbcConnect(database, uid = uid, pwd = pwd)
  
  standards <- sqlQuery(db, paste("select * from ", "standards"))

  #add process type
  ps <- read.csv("std_process.csv")
  standards <- inner_join(standards, ps, by = "rec_num") 
  
  standards <- standards %>%
    mutate(fm_exp = ifelse(!is.na(Fm_cons), Fm_cons, Fm_NOSAM_avg)) %>%
    select(rec_num, sample_id, process, fm_exp)
    
  #create factor of tiri_id, order by Fm
  standards <- within(standards, name <- factor(sample_id, levels = unique(
                   sample_id[order(fm_exp, sample_id)]),ordered = TRUE))
  
  RODBC::odbcClose(db)
  
  return(standards)
}

mungeStandards <- function(data, std) {
    
  #join standard table
  data <- inner_join(data, std, by = "rec_num")
  
  out <- data %>% 
    mutate(rep_err = pmax(f_int_error, f_ext_error),
           normFm = normFm(f_modern, fm_exp),
           sigma = sigma(f_modern, fm_exp, rep_err),
           frep_err = rep_err/f_modern, 
           #system
           system = ifelse(grepl("CFAMS", wheel_id), "CFAMS", "USAMS"),
           #fix CFAMS 12C
           le12c = ifelse(system == "USAMS", le12c * -1, le12c),
           #is primary?
           primary = ifelse(!is.na(sample_type_1) & sample_type_1 == "S",
                            TRUE, 
                            ifelse(!is.na(sample_type) & sample_type == "S", 
                                   TRUE, FALSE))) %>%
    select(-target_name, -f_int_error, -f_ext_error, -sample_type, 
           -sample_type_1) %>%
    #number of splits?
    group_by(osg_num) %>% #For each osg_num
    mutate(splits = n()) #Count occurrences to get number of splits

  return(out)
}

getQCData <- function(from, to, sys) {
  # Function to get standards from database and return munged table
  
std <- getStdTable()

data <- getStandards(from, to, sys)

out <- mungeStandards(data, std)

}


