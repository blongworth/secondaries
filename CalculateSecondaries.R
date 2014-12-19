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
  sqlQuery(db, paste("
        SELECT target.rec_num, target.tp_num, target.osg_num, target.target_name, 
          wheel_pos.wheel_id, target.tp_date_pressed, graphite_lab.lab_name, 
          no_os.f_modern, no_os.f_int_error, no_os.f_ext_error, 
          graphite.gf_co2_qty, no_os.q_flag
        FROM no_os, target, wheel_pos, graphite, graphite_lab
        WHERE target.tp_num = no_os.tp_num AND target.tp_num = wheel_pos.tp_num 
          AND graphite.osg_num = target.osg_num 
          AND target.graphite_lab = graphite_lab.lab_id
          AND target.rec_num =", rec," 
          ",whid, "
		      AND target.tp_date_pressed > '",from,"'
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
    m$fmd <- m$f_modern - fmc
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
