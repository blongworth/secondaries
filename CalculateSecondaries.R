#'This script reads secondary data and consensus values
#'from the NOSAMS db and produces a series of plots. It
#'can be used to select data for a given system and time
#'period. 

# #Run Parameters
# from <- '2012-01-01'
# to <- 'present' #present or date
# system <- 'cfams' #cfams or ams1


####
#Define functions
####

#Get secondary from database
getSecondary <- function (rec = 87012, from = "2013-02-01", to = "present", sys = "ams1") {
  
  require(RODBC) 
  
  if (sys == "cfams") {
    whid <- "AND wheel_id LIKE 'C%'"
  } else if (sys =="usams") {
    whid <- "AND wheel_id LIKE 'U%'"
  } else if (sys =="both") {
    whid <- "" 
  } else {
    whid <- "AND wheel_id NOT LIKE 'C%'"
  }
  
  if (to != "present") {
    ts <- paste("AND target.tp_date_pressed < '", to,"' ")
  } else {
    ts <- ""
  }
  
  #Open DB connection
  nosams <- odbcConnect(database, uid = uid, pwd = pwd)
  
  #Do the query
  x <- sqlQuery(nosams, paste("
        SELECT target.rec_num, target.tp_num, target.osg_num, target.target_name, wheel_pos.wheel_id, 
          target.tp_date_pressed, graphite_lab.lab_name, no_os.f_modern, 
          no_os.f_int_error, no_os.f_ext_error, graphite.gf_co2_qty
        FROM no_os, target, wheel_pos, graphite, graphite_lab
        WHERE target.tp_num = no_os.tp_num AND target.tp_num = wheel_pos.tp_num 
          AND graphite.osg_num = target.osg_num 
          AND target.graphite_lab = graphite_lab.lab_id
  	      AND target.rec_num =", rec," 
          ",whid, "
		      AND target.tp_date_pressed > '",from,"'
            ", ts, "
          "))
  
  #Close DB connection
  odbcClose(nosams)
  
  x
}

#Calculate Sigma
sigma <- function(fmm,fmc,em,ec) {
  (fmm - fmc) /sqrt(em^2 + ec^2)
}

#Calculate normalized fm
normFm <- function (fmm, fmc) {
  (fmm - fmc) / fmc
}

#calculate secondary sigmas using intcal results
calcSecondary <- function (rec, from = '2013-02-01', to = 'present', sys = 'nosams') {
  
  #get secondary data and calculate sigmas, checking for NA's
  m <- getSecondary(rec, from, to, sys)
  
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

#calculate secondary sigmas using standard table results
calcSecondaryStd <- function (rec, from = '2013-02-01', to = 'present', sys = 'nosams') {
  
  #get secondary data and calculate sigmas, checking for NA's
  m <- getSecondary(rec, from, to, sys)
  
  if(dim(m)[1] > 0) {
    #get consensus value
    fmc <- as.numeric(subset(standards,rec_num == rec, fm_consensus))
    #get reported error
    m$merr <- pmax(m$f_int_err,m$f_ext_err)
    
    m <- within(m, sigma <- sigma(f_modern, standards[standards$rec_num == rec, 8], merr, 0))
    m$fmd <- m$f_modern - fmc
    m$normFm <- normFm(m$f_modern, fmc)
    m
  }
  else {
    return()
  }
}

