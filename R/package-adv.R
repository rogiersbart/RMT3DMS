#' Read an MT3DMS advection package file
#' 
#' \code{read_adv} reads in an MT3DMS advection package file and returns it as an \code{\link{RMT3DMS}} adv object.
#' 
#' @param file filename; typically '*.adv'
#' @return object of class adv
#' @importFrom readr read_lines
#' @export
rmt_read_adv <- function(file = {cat('Please select adv file ...\n'); file.choose()}) {
  
  adv_lines <- read_lines(file)
  adv <- NULL
  
  # Data set B1
  data_set_b1 <- remove_empty_strings(strsplit(adv_lines[1],' ')[[1]])
  adv_lines <- adv_lines[-1]  
  adv$mixelm <- as.numeric(data_set_b1[1])
  adv$percel <- as.numeric(data_set_b1[2])
  adv$mxpart <- as.numeric(data_set_b1[3])
  adv$nadvfd <- as.numeric(data_set_b1[4])
  rm(data_set_b1)
  
  # Data set B2
  if(adv$mixelm %in% c(1,2,3)) {  
    data_set_b2 <- remove_empty_strings(strsplit(adv_lines[1],' ')[[1]])
    adv_lines <- adv_lines[-1]  
    adv$itrack <- as.numeric(data_set_b2[1])
    adv$wd <- as.numeric(data_set_b2[2])
    rm(data_set_b2)
  }
  
  # Data set B3
  if(adv$mixelm %in% c(1,3)) {  
    data_set_b3 <- remove_empty_strings(strsplit(adv_lines[1],' ')[[1]])
    adv_lines <- adv_lines[-1]  
    adv$dceps <- as.numeric(data_set_b3[1])
    adv$nplane <- as.numeric(data_set_b3[2])
    adv$npl <- as.numeric(data_set_b3[3])
    adv$nph <- as.numeric(data_set_b3[4])
    adv$npmin <- as.numeric(data_set_b3[5])
    adv$npmax <- as.numeric(data_set_b3[6])
    rm(data_set_b3)
  }
  
  # Data set B4
  if(adv$mixelm %in% c(2,3)) {  
    data_set_b4 <- remove_empty_strings(strsplit(adv_lines[1],' ')[[1]])
    adv_lines <- adv_lines[-1]  
    adv$interp <- as.numeric(data_set_b4[1])
    adv$nlsink <- as.numeric(data_set_b4[2])
    adv$npsink <- as.numeric(data_set_b4[3])
    rm(data_set_b4)
  }
  
  # Data set B5
  if(adv$mixelm==3) {  
    adv$dchmoc <- as.numeric(remove_empty_strings(strsplit(adv_lines[1],' ')[[1]]))
    adv_lines <- adv_lines[-1]  
  }
  
  class(adv) <- c('adv','mt3dms_package')
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
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmf_write_adv <- function(adv,
                      file = {cat('Please select adv file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1) {
  
  # Data set B1
  cat(paste0(c(prettyNum(c(adv$mixelm,adv$percel,adv$mxpart,adv$nadvfd),width=10), '\n'),collapse=''), file=file)
  
  # Data set B2
  if(adv$mixelm %in% c(1,2,3)) {  
    cat(paste0(c(prettyNum(c(adv$itrack,adv$wd),width=10), '\n'),collapse=''), file=file, append=TRUE)
  }
  
  # Data set B3
  if(adv$mixelm %in% c(1,3)) {  
    cat(paste0(c(prettyNum(c(adv$dceps,adv$nplane,adv$npl,adv$nph,adv$npmin,adv$npmax),width=10), '\n'),collapse=''), file=file, append=TRUE)
  }
  
  # Data set B4
  if(adv$mixelm %in% c(2,3)) {  
    cat(paste0(c(prettyNum(c(adv$interp,adv$nlsink,adv$npsink),width=10), '\n'),collapse=''), file=file, append=TRUE)
  }
  
  # Data set B5
  if(adv$mixelm==3) {  
    cat(paste0(c(prettyNum(c(adv$dchmoc),width=10), '\n'),collapse=''), file=file, append=TRUE)
  }  
}

#' @describeIn rmf_write_adv Deprecated function name
#' @export
write_adv <- function(...) {
  .Deprecated(new = "rmf_write_adv", old = "write_adv")
  rmf_write_adv(...)
}