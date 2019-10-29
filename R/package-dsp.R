#' Read an MT3DMS dispersion package file
#' 
#' \code{read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file filename; typically '*.dsp'
#' @param btn basic transport package file object
#' @return object of class dsp
#' @importFrom readr read_lines
#' @export
rmt_read_dsp <- function(file = {cat('Please select dsp file ...\n'); file.choose()},
                     btn = read_btn()) {
  
  dsp.lines <- read_lines(file)
  dsp <- NULL
  
  # multidiffusion option
  multiDiffusionOption <- remove_empty_strings(strsplit(dsp.lines[1],' ')[[1]])
  if(multiDiffusionOption[2]=='MultiDiffusion')
  {
    dsp$multidiffusion <- TRUE
    dsp.lines <- dsp.lines[-1]
  }
  
  # Data set C1
  data_set_c1 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
  dsp$al <- data_set_c1$array
  dsp.lines <- data_set_c1$remaining_lines
  rm(data_set_c1)
  
  # Data set C2
  data_set_c2 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
  dsp$trpt <- data_set_c2$array
  dsp.lines <- data_set_c2$remaining_lines
  rm(data_set_c2)
  
  # Data set C3
  data_set_c3 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
  dsp$trpv <- data_set_c3$array
  dsp.lines <- data_set_c3$remaining_lines
  rm(data_set_c3)
  
  # Data set C4
  if(dsp$multidiffusion) {
    dsp$dmcoef <- list()
    for(comp in 1:btn$ncomp) {
      data_set_c4 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
      dsp$dmcoef[[comp]] <- data_set_c4$array
      dsp.lines <- data_set_c4$remaining_lines
    }
    rm(data_set_c4)
  } else {
    data_set_c4 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
    dsp$dmcoef <- data_set_c4$array
    dsp.lines <- data_set_c4$remaining_lines
    rm(data_set_c4)
  }
  
  class(dsp) <- c('dsp','mt3dms_package')
  return(dsp)
}

#' @describeIn rmt_read_dsp Deprecated function name
#' @export
read_dsp <- function(...) {
  .Deprecated(new = "rmt_read_dsp", old = "read_dsp")
  rmt_read_dsp(...)
}

#' Write an MT3DMS dispersion package file
#' 
#' @param dsp an \code{\link{RMT3DMS}} dsp object
#' @param file filename to write to; typically '*.dsp'
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmt_write_dsp <- function(dsp,
                      file = {cat('Please select dsp file to overwrite or provide new filename ...\n'); file.choose()},
                      btn,
                      iprn=-1) {
  
  if(dsp$multidiffusion) {
    cat('$ MultiDiffusion\n',file=file)
  }
  
  # Data set C1    
  for(i in 1:dim(dsp$al)[3]) {
    if(i==1 & !dsp$multidiffusion) cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file)
    if(i==1 & dsp$multidiffusion) cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
    if(i!=1) cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
    write.table(dsp$al[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
  }
  
  # Data set C2
  cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
  cat(paste(paste(dsp$trpt, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C3
  cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
  cat(paste(paste(dsp$trpv, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C4
  if(dsp$multidiffusion) {
    for(comp in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(dsp$dmcoef[[comp]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  } else {
    cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(dsp$dmcoef, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  }
}

#' @describeIn rmt_write_dsp Deprecated function name
#' @export
write_dsp <- function(...) {
  .Deprecated(new = "rmt_write_dsp", old = "write_dsp")
  rmt_write_dsp(...)
}