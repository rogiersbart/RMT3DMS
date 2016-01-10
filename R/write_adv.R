#' Write an MT3DMS advection package file
#' 
#' @param adv an \code{\link{RMT3DMS}} adv object
#' @param file filename to write to; typically '*.adv'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_adv <- function(adv,
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
