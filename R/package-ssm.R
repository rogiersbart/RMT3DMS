#' Read an MT3DMS sink & source mixing package file
#' 
#' \code{rmt_read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \code{\link{RMT3DMS}} ssm object.
#' 
#' @param file filename; typically '*.ssm'
#' @param btn \code{RMT3DMS} btn object
#' @return object of class \code{ssm}
#' @export
rmt_read_ssm <- function(file = {cat('Please select ssm file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())}) {
  
  ssm_lines <- read_lines(file)
  ssm <- NULL
  
  # Data set D1
  data_set_d1 <- as.logical(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
  ssm$fwel <- data_set_d1[1]
  ssm$fdrn <- data_set_d1[2]
  ssm$frch <- data_set_d1[3]
  ssm$fevt <- data_set_d1[4]
  ssm$friv <- data_set_d1[5]
  ssm$fghb <- data_set_d1[6]
  ssm$fnew <- data_set_d1[7:10]
  ssm_lines <- ssm_lines[-1]  
  
  # Data set D2
  ssm$mxss <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
  ssm_lines <- ssm_lines[-1]  
  
  ssm$incrch <- NULL
  ssm$crch <- list()
  ssm$incevt <- NULL
  ssm$cevt <- list()
  ssm$nss <- NULL
  ssm$kss <- list()
  ssm$iss <- list()
  ssm$jss <- list()
  ssm$css <- list()
  ssm$itype <- list()
  ssm$cssms <- list()
  
  for(stress_period in 1:btn$nper) {
    # Data set D3
    if(ssm$frch) {
      ssm$incrch[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
      ssm_lines <- ssm_lines[-1] 
      
      # Data set D4
      if(ssm$frch & (ssm$incrch[stress_period] >= 0)) {
        data_set_d4 <- read_mt3dms_array(ssm_lines,btn$nrow,btn$ncol,btn$ncomp)
        ssm_lines <- data_set_d4$remaining_lines
        ssm$crch[[stress_period]] <- data_set_d4$array
        rm(data_set_d4)
      }
    }
    
    # Data set D5
    if(ssm$fevt) {
      ssm$incevt[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
      ssm_lines <- ssm_lines[-1] 
      
      # Data set D6
      if(ssm$fevt & (ssm$incevt[stress_period] >= 0)) {
        data_set_d6 <- read_mt3dms_array(ssm_lines,btn$nrow,btn$ncol,btn$ncomp)
        ssm_lines <- data_set_d6$remaining_lines
        ssm$cevt[[stress_period]] <- data_set_d6$array
        rm(data_set_d6)
      }
    }
    
    # Data set D7
    ssm$nss[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
    ssm_lines <- ssm_lines[-1] 
    
    ssm$kss[[stress_period]] <- rep(NA,ssm$nss[stress_period])
    ssm$iss[[stress_period]] <- ssm$kss
    ssm$jss[[stress_period]] <- ssm$kss
    ssm$css[[stress_period]] <- ssm$kss
    ssm$itype[[stress_period]] <- ssm$kss
    ssm$cssms[[stress_period]] <- list()
    
    # Data set D8
    if(ssm$nss > 0) {
      for(i in 1:ssm$nss[stress_period]) {
        data_set_d8 <- as.logical(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
        ssm$kss[[stress_period]][i] <- data_set_d8[1]
        ssm$iss[[stress_period]][i] <- data_set_d8[2]
        ssm$jss[[stress_period]][i] <- data_set_d8[3]
        ssm$css[[stress_period]][i] <- data_set_d8[4]
        ssm$itype[[stress_period]][i] <- data_set_d8[5]
        ssm$cssms[[stress_period]][[i]] <- data_set_d8[6:(6+btn$ncomp-1)]
        ssm_lines <- ssm_lines[-1] 
      }
    }
  }
  
  class(ssm) <- c('ssm','mt3dms_package')
  return(ssm)
}

#' Write an MT3DMS Sink-Source Mixing Package file
#' 
#' @param ssm an \code{RMT3DMS} ssm object
#' @param file filename to write to; typically '*.ssm'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmt_write_ssm <- function(ssm,
                          file = {cat('Please select ssm file to overwrite or provide new filename ...\n'); file.choose()},
                          btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                          iprn=-1) {
  
  # Data set D1
  cat(paste0(c(as.character(factor(c(ssm$fwel,ssm$fdrn,ssm$frch,ssm$fevt,ssm$friv,ssm$fghb,ssm$fnew),levels=c(TRUE,FALSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file)
  
  # Data set D2
  cat(paste0(c(prettyNum(c(ssm$mxss),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  for(stress_period in 1:btn$nper) {
    
    # Data set D3
    if(ssm$frch) {
      cat(paste0(c(prettyNum(c(ssm$incrch[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      
      # Data set D4
      if(ssm$frch & (ssm$incrch[stress_period] >= 0)) {
        for(i in 1:btn$ncomp) {
          cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
          if(btn$ncomp>1) write.table(ssm$crch[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
          if(btn$ncomp==1) write.table(ssm$crch[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
    
    # Data set D5
    if(ssm$fevt) {
      cat(paste0(c(prettyNum(c(ssm$incevt[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      
      # Data set D6
      if(ssm$fevt & (ssm$incevt[stress_period] >= 0)) {
        for(i in 1:btn$ncomp) {
          cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
          if(btn$ncomp>1) write.table(ssm$cevt[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
          if(btn$ncomp==1) write.table(ssm$cevt[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
    
    # Data set D7
    cat(paste0(c(prettyNum(c(ssm$nss[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set D8
    if(ssm$nss > 0) {
      for(i in 1:ssm$nss[stress_period]) {
        if(btn$ncomp > 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i],ssm$cssms[[stress_period]][[i]]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        if(btn$ncomp == 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      }
    }
  }
}
