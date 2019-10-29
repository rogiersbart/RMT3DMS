#' Read an MT3DMS mass balance summary file
#' 
#' \code{read_mas} reads in an MT3DMS mass balance summary file and returns it as an \code{\link{RMT3DMS}} mas object.
#' 
#' @param file filename; typically '*.mas'
#' @return object of class mas
#' @export
rmt_read_mas <- function(file = {cat('Please select mas file ...\n'); file.choose()}) {
  
  mas <- read.table(file,skip=2)
  names(mas) <- c('TIME','TOTal_IN','TOTal_OUT','SOURCES','SINKS','NET_MASS_FROM_FLUID_STORAGE','TOTal_MASS_IN_AQUIFER','DISCREPANCY_TOTal_IN_OUT','DISCREPANCY_alTERNATIVE')
  class(mas) <- c('mas','mt3dms_package')
  return(mas)
}

#' @describeIn rmt_read_mas Deprecated function name
#' @export
read_mas <- function(...) {
  .Deprecated(new = "rmt_read_mas", old = "read_mas")
  rmt_read_mas(...)
}

#' Read an MT3DMS unformatted concentration file
#' 
#' \code{read_ucn} reads in an MT3DMS unformatted concentration file and returns it as an \code{\link{RMT3DMS}} ucn object.
#' 
#' @param file filename; typically '*.ucn'
#' @param btn basic transport package file object
#' @return object of class ucn
#' @export
rmt_read_ucn <- function(file = {cat('Please select ucn file ...\n'); file.choose()},
                     btn = rmt_read_btn()) {
  
  ucn <- NULL
  ucn$ntrans <- list()
  ucn$kstp <- list()
  ucn$kper <- list()
  ucn$time2 <- list()
  ucn$text <- list()
  ucn$ncol <- list()
  ucn$nrow <- list()
  ucn$ilay <- list()
  ucn$cnew <- list()
  con <- file(file,open='rb')
  for(transport_step in 1:ifelse(btn$nprs<=0,1,btn$nprs)) {
    ucn$ntrans[[transport_step]] <- rep(NA,btn$nlay)
    ucn$kstp[[transport_step]] <- rep(NA,btn$nlay)
    ucn$kper[[transport_step]] <- rep(NA,btn$nlay)
    ucn$time2[[transport_step]] <- rep(NA,btn$nlay)
    ucn$text[[transport_step]] <- rep(NA,btn$nlay)
    ucn$ncol[[transport_step]] <- rep(NA,btn$nlay)
    ucn$nrow[[transport_step]] <- rep(NA,btn$nlay)
    ucn$ilay[[transport_step]] <- rep(NA,btn$nlay)
    ucn$cnew[[transport_step]] <- array(NA,dim=c(btn$nrow,btn$ncol,btn$nlay))
    for(k in 1:btn$nlay) {
      # Record 1
      ucn$ntrans[[transport_step]][k] <- readBin(con,what='integer',n=1)
      ucn$kstp[[transport_step]][k] <- readBin(con,what='integer',n=1)
      ucn$kper[[transport_step]][k] <- readBin(con,what='integer',n=1)
      ucn$time2[[transport_step]][k] <- readBin(con,what='numeric',n=1,size=4)
      ucn$text[[transport_step]][k] <- readChar(con,nchars=16)
      ucn$ncol[[transport_step]][k] <- readBin(con,what='integer',n=1)
      ucn$nrow[[transport_step]][k] <- readBin(con,what='integer',n=1)
      ucn$ilay[[transport_step]][k] <- readBin(con,what='integer',n=1)
      
      # Record 2
      ucn$cnew[[transport_step]][,,k] <- matrix(readBin(con,what='numeric',n=btn$ncol*btn$nrow,size=4),nrow=btn$nrow,ncol=btn$ncol,byrow=TRUE)
    }
    class(ucn$cnew[[transport_step]]) <- 'rmt3dms_3d_array'
    ucn$cnew[[transport_step]][which(ucn$cnew[[transport_step]]==btn$cinact)] <- NA
  }
  close(con)
  class(ucn) <- c('ucn','mt3dms_package')
  return(ucn)
}

#' @describeIn rmt_read_ucn Deprecated function name
#' @export
read_ucn <- function(...) {
  .Deprecated(new = "rmt_read_ucn", old = "read_ucn")
  rmt_read_ucn(...)
}
