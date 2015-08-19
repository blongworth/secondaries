#Functions for secondary standard analysis

####
#Define functions
####


#Calculate Sigma
sigma <- function(fmm,fmc,em,ec) {
  (fmm - fmc) /sqrt(em^2 + ec^2)
}


#Calculate normalized fm
normFm <- function (fmm, fmc) {
  (fmm - fmc) / fmc
}

#calculate intrinsic error
intrErr <- function(totErr, targErr) {
  ifelse (totErr < targErr, NA, sqrt(totErr^2 - targErr^2))
}

#Calculate total error
totErr <- function(targErr, intrErr) {
  sqrt(targErr^2 + intrErr^2)
}



getSecondary <- function (rec, from, to, sys, db) { 
  #Get data for a secondary from database
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
  
  #Do the query
  f <- sqlQuery(db, paste("
        SELECT target.rec_num, target.tp_num, target.osg_num, target.target_name, 
          wheel_pos.wheel_id, target.tp_date_pressed, graphite_lab.lab_name, 
          no_os.f_modern, no_os.f_int_error, no_os.f_ext_error, no_os.dc13,
          graphite.gf_co2_qty, no_os.q_flag, snics_results.sample_type, snics_results.sample_type_1
        FROM no_os, target, wheel_pos, graphite, graphite_lab, snics_results
        WHERE target.tp_num = no_os.tp_num 
          AND target.tp_num = wheel_pos.tp_num 
          AND target.tp_num = snics_results.tp_num
          AND target.osg_num = graphite.osg_num 
          AND target.graphite_lab = graphite_lab.lab_id
          AND target.rec_num =", rec," 
          ",whid, "
		      AND target.tp_date_pressed > '",from,"'
          ", ts, "
        "))
  
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

  f <- left_join(f, cur, by = "tp_num")
  left_join(f, count, by = "tp_num")
}

getOldSecondary <- function (rec, from, to, sys, db) { 
  #Get data for a secondary from database
  #Return query result as data table
  
#   #What system do we want data for?
#   if (sys == "cfams") {
#     whid <- "AND wheel_id LIKE 'C%'"
#   } else if (sys =="usams") {
#     whid <- "AND wheel_id LIKE 'U%'"
#   } else if (sys =="both") {
#     whid <- "" 
#   } else {
#     whid <- "AND wheel_id NOT LIKE 'C%'"
#   }
  
  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }
  
  #Do the query
  sqlQuery(db, paste("
                          SELECT target.rec_num, target.tp_num, target.osg_num, target.target_name, 
                          wheel_pos.wheel_id, target.tp_date_pressed, graphite_lab.lab_name, 
                          no_os.f_modern, no_os.f_int_error, no_os.f_ext_error, no_os.dc13,
                          graphite.gf_co2_qty, no_os.q_flag
                          FROM no_os, target, wheel_pos, graphite, graphite_lab
                          WHERE target.tp_num = no_os.tp_num 
                          AND target.tp_num = wheel_pos.tp_num 
                          AND target.tp_num = snics_results.tp_num
                          AND target.osg_num = graphite.osg_num 
                          AND target.graphite_lab = graphite_lab.lab_id
                          AND target.rec_num =", rec," 
                          AND target.tp_date_pressed > '",from,"'
                          ", ts, "
                          "))
  
  
}

getSecondaryQC <- function (rec, from, to, sys, db) { 
  #Get data for a secondary from database
  #Return query result as data table
  
  #What system do we want data for?
  if (sys == "cfams") {
    whid <- "AND wheel LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel LIKE 'U%'"
  } else if (sys =="both") {
    whid <- "" 
  } else {
    whid <- "AND wheel NOT LIKE 'C%'"
  }
  
  #Data to present or provided end date
  if (to != "present") {
    ts <- paste("AND target_time < '", to,"' ")
  } else {
    ts <- ""
  }
  
  #Do the query
  sqlQuery(db, paste("
        SELECT rec_num, tp_num, num, target_name, 
          wheel, target_time, lab, 
          f_modern, f_int_error, f_ext_error, 
          gf_co2_qty, q_flag
        FROM qc
        WHERE rec_num =", rec," 
          ",whid, "
  	      AND target_time > '",from,"'
            ", ts, "
          "))
  
}

calcSecondary <- function (rec, from, to, sys, intcal, db) {
  #Get data for a secondary standard and calculate fmdiff and
  #sigma  using intcal results
  
  #get secondary data and calculate sigmas, checking for NA's
  m <- getSecondary(rec, from, to, sys, db)
  
  if(dim(m)[1] > 0) {
    fmc <- as.numeric(subset(intcal,rec_num == rec, fm_consensus))
    m$merr <- pmax(m$f_int_err,m$f_ext_err)
    m<- within(m, sigma <- sigma(f_modern, intcal[intcal$rec_num == rec, 4], merr, 
                                 intcal[intcal$rec_num == rec, 15]))
    #m$fmd <- m$f_modern - fmc
    m$normFm <- normFm(m$f_modern, fmc)
    m
  }
  else {
    return()
  }
}


#Main function for getting secondaries
calcSecondaries <- function (from, to, sys, intcal, db) {
  #For all secondary standard rec_nums in a dataframe,
  #Get secondary data from database and calculate fmdiff
  #and sigma
  
  #Create data frame of all secondaries with sigmas
  out <- lapply(X = as.list(intcal$rec_num), FUN = calcSecondary, from, to, sys, intcal, db)
  out <- do.call("rbind", out)
    
  #clean it up
  out$system[grepl("CFAMS", out$wheel_id)] <- "CFAMS"
  out$system[grepl("USAMS", out$wheel_id)] <- "USAMS"
  out$system[is.na(out$system)] <- "AMS1"
  merge (out,intcal[, c("rec_num", "name", "fm_consensus")], by= "rec_num")
    
}

#Get QC data from database as data frame
getQCData <- function (from, to, sys) {
  library(RODBC)
  #Open DB connection
  db <- odbcConnect(database, uid = uid, pwd = pwd)
  
  ###
  #Make intcal table
  ###
  
  #get intcal table
  
  intcal <- sqlQuery(db, paste("select * from ", "intercal_samples"))
  
  #create factor of tiri_id, order by Fm
  intcal <- within(intcal, name <- factor(tiri_id, levels = unique(
    tiri_id[order(fm_consensus, tiri_id)]),ordered = TRUE))
  
  #Replace C-6 with new consensus from Xiaomei 2010
  intcal$fm_consensus[intcal$rec_num == 1086] <- 1.5016
  
  #add in OX-I, OX-II
  ox <- read.csv("intcalox.csv")
  intcal <- rbind(intcal, ox)
  
  
  ###
  #get secondary data
  ###
  
  out <- calcSecondaries(from, to, sys, intcal, db)
  
  #Close DB
  odbcClose(db)
  
  #function to determine primaries
  prim <- function(x) {
    if (!is.na(x)) {
      
    }
  }
  #Filter and munge
  out.t <- out %>% 
    filter(is.na(q_flag), #Check for q_flag
           sigma < 10, sigma > -10, #Select reasonable sigmas
           normFm < 0.05, normFm > -0.05) %>% #Select reasonable Fm
    mutate(finterr = f_int_error/f_modern, #add %err
           fexterr = f_ext_error/f_modern, 
           #errrat = merr/abs(fmd),
           fmaxerr = merr/f_modern, 
           le12c = ifelse(system == "USAMS", le12c * -1, le12c),
           primary = ifelse(!is.na(sample_type_1) & sample_type_1 == "S",
                            TRUE, 
                            ifelse(!is.na(sample_type) & sample_type == "S", 
                                   TRUE, FALSE))) %>%
    filter(fmaxerr < 0.10) %>% 
    group_by(osg_num) %>% #For each osg_num
    mutate(splits = n()) #Count occurrences to get number of splits
     
}

