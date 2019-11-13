#' Read an MT3DMS advection package file
#' 
#' \code{rmt_read_adv} reads in an MT3DMS advection package file and returns it as an \code{\link{RMT3DMS}} adv object.
#' 
#' @param file filename; typically '*.adv'
#' @return object of class adv
#' @export
rmt_read_adv <- function(file = {cat('Please select adv file ...\n'); file.choose()}) {
  
  adv_lines <- readr::read_lines(file)
  adv <- list()
  
  # Data set 1
  data_set_1 <- rmti_parse_variables(adv_lines, n = 4)
  adv$mixelm <- as.numeric(data_set_1$variables[1])
  adv$percel <- as.numeric(data_set_1$variables[2])
  if(adv$mixelm %in% c(1,3)) adv$mxpart <- as.numeric(data_set_1$variables[3])
  if(adv$mixelm == 0) {
    if(is.na(suppressWarnings(as.numeric(data_set_1$variables[4])))) {
      adv$nadvfd <- 0
    } else {
      adv$nadvfd <- as.numeric(data_set_1$variables[4])
    }
  } 
  adv_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2
  if(adv$mixelm %in% c(1,2,3)) {  
    data_set_2 <- rmti_parse_variables(adv_lines, n = 2)
    adv$itrack <- as.numeric(data_set_2$variables[1])
    adv$wd <- as.numeric(data_set_2$variables[2])
    adv_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  }
  
  # Data set 3
  if(adv$mixelm %in% c(1,3)) {  
    data_set_3 <- rmti_parse_variables(adv_lines, n = 6)
    adv$dceps <- as.numeric(data_set_3$variables[1])
    adv$nplane <- as.numeric(data_set_3$variables[2])
    adv$npl <- as.numeric(data_set_3$variables[3])
    adv$nph <- as.numeric(data_set_3$variables[4])
    adv$npmin <- as.numeric(data_set_3$variables[5])
    adv$npmax <- as.numeric(data_set_3$variables[6])
    adv_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  }
  
  # Data set 4
  if(adv$mixelm %in% c(2,3)) {  
    data_set_4 <- rmti_parse_variables(adv_lines, n = 3)
    adv$interp <- as.numeric(data_set_4$variables[1])
    adv$nlsink <- as.numeric(data_set_4$variables[2])
    adv$npsink <- as.numeric(data_set_4$variables[3])
    adv_lines <- data_set_4$remaining_lines
    rm(data_set_4)
  }
  
  # Data set 5
  if(adv$mixelm == 3) {  
    data_set_5 <- rmti_parse_variables(adv_lines, n = 1)
    adv$dchmoc <- as.numeric(data_set_5$variables[1])
    adv_lines <- data_set_5$remaining_lines
  }
  
  class(adv) <- c('adv','rmt_package')
  return(adv)
}

#' @describeIn rmt_read_adv Deprecated function name
#' @export
read_adv <- function(...) {
  .Deprecated(new = "rmt_read_adv", old = "read_adv")
  rmt_read_adv(...)
}

#' Write an MT3DMS advection package file
#' 
#' @param adv an \code{\link{RMT3DMS}} adv object
#' @param file filename to write to; typically '*.adv'
#' @return \code{NULL}
#' @export
rmt_write_adv <- function(adv,
                      file = {cat('Please select adv file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # Data set 1
  rmti_write_variables(adv$mixelm, adv$percel, ifelse(adv$mixelm %in% c(1,3), adv$mxpart, ''), ifelse(adv$mixelm == 0, adv$nadvfd, ''), file = file, append = FALSE)
  
  # Data set 2
  if(adv$mixelm %in% c(1,2,3)) {  
    rmti_write_variables(adv$itrack, adv$wd, file = file)
  }
  
  # Data set 3
  if(adv$mixelm %in% c(1,3)) {  
    rmti_write_variables(adv$dceps, adv$nplane, adv$npl, adv$nph, adv$npmin, adv$npmax, file = file)
  }
  
  # Data set 4
  if(adv$mixelm %in% c(2,3)) {  
    rmti_write_variables(adv$interp, adv$nlsink, adv$npsink, file = file)
  }
  
  # Data set 5
  if(adv$mixelm == 3) {  
    rmti_write_variables(adv$dchmoc, file = file)
  }  
}

#' @describeIn rmt_write_adv Deprecated function name
#' @export
write_adv <- function(...) {
  .Deprecated(new = "rmt_write_adv", old = "write_adv")
  rmt_write_adv(...)
}