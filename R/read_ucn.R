#' Read an MT3DMS unformatted concentration file
#' 
#' \code{read_ucn} reads in an MT3DMS unformatted concentration file and returns it as an \code{\link{RMT3DMS}} ucn object.
#' 
#' @param file filename; typically '*.ucn'
#' @param btn basic transport package file object
#' @return object of class ucn
#' @export
read_ucn <- function(file,btn)
{
  ucn <- NULL
  ucn$NTRANS <- list()
  ucn$KSTP <- list()
  ucn$KPER <- list()
  ucn$TIME2 <- list()
  ucn$TEXT <- list()
  ucn$ncol <- list()
  ucn$nrow <- list()
  ucn$ILAY <- list()
  ucn$CNEW <- list()
  con <- file(file,open='rb')
  for(transport_step in 1:ifelse(btn$nprs<=0,1,btn$nprs))
  {
    ucn$NTRANS[[transport_step]] <- rep(NA,btn$nlay)
    ucn$KSTP[[transport_step]] <- rep(NA,btn$nlay)
    ucn$KPER[[transport_step]] <- rep(NA,btn$nlay)
    ucn$TIME2[[transport_step]] <- rep(NA,btn$nlay)
    ucn$TEXT[[transport_step]] <- rep(NA,btn$nlay)
    ucn$ncol[[transport_step]] <- rep(NA,btn$nlay)
    ucn$nrow[[transport_step]] <- rep(NA,btn$nlay)
    ucn$ILAY[[transport_step]] <- rep(NA,btn$nlay)
    ucn$CNEW[[transport_step]] <- array(NA,dim=c(btn$nrow,btn$ncol,btn$nlay))
    for(k in 1:btn$nlay)
    {
      # Record 1
        ucn$NTRANS[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$KSTP[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$KPER[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$TIME2[[transport_step]][k] <- readBin(con,what='numeric',n=1,size=4)
        ucn$TEXT[[transport_step]][k] <- readChar(con,nchars=16)
        ucn$ncol[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$nrow[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$ILAY[[transport_step]][k] <- readBin(con,what='integer',n=1)
      
      # Record 2
        ucn$CNEW[[transport_step]][,,k] <- matrix(readBin(con,what='numeric',n=btn$ncol*btn$nrow,size=4),nrow=btn$nrow,ncol=btn$ncol,byrow=TRUE)
    }
    class(ucn$CNEW[[transport_step]]) <- 'rmt3dms_3d_array'
    ucn$CNEW[[transport_step]][which(ucn$CNEW[[transport_step]]==btn$CINACT)] <- NA
  }
  close(con)
  class(ucn) <- c('ucn','mt3dms_package')
  return(ucn)
}
