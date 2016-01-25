getSecondaries <- function (from, to, sys, db) { 
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
                          "))

  cur <- sqlQuery(db, paste("
                            SELECT 
                              snics_raw.tp_num, 
                              AVG(le12c) AS le12c,
                              SUM(cnt_14c) AS counts
                            FROM snics_raw
                              INNER JOIN target
                                ON snics_raw.tp_num = target.tp_num
                            WHERE
                              ok_calc = 1
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
