#' Read an MT3DMS unformatted concentration file
#' 
#' \code{read.ucn} reads in an MT3DMS unformatted concentration file and returns it as an \code{\link{RMT3DMS}} ucn object.
#' 
#' @param file Filename; typically *.ucn
#' @return Object of class ucn
#' @export
read.ucn <- function(file,btn)
{
  ucn <- NULL
  ucn$NTRANS <- list()
  ucn$KSTP <- list()
  ucn$KPER <- list()
  ucn$TIME2 <- list()
  ucn$TEXT <- list()
  ucn$NCOL <- list()
  ucn$NROW <- list()
  ucn$ILAY <- list()
  ucn$CNEW <- list()
  con <- file(file,open='rb')
  for(transport_step in 1:ifelse(btn$NPRS<=0,1,btn$NPRS))
  {
    ucn$NTRANS[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$KSTP[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$KPER[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$TIME2[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$TEXT[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$NCOL[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$NROW[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$ILAY[[transport_step]] <- rep(NA,btn$NLAY)
    ucn$CNEW[[transport_step]] <- array(NA,dim=c(btn$NROW,btn$NCOL,btn$NLAY))
    for(k in 1:btn$NLAY)
    {
      # Record 1
        ucn$NTRANS[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$KSTP[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$KPER[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$TIME2[[transport_step]][k] <- readBin(con,what='numeric',n=1,size=4)
        ucn$TEXT[[transport_step]][k] <- readChar(con,nchars=16)
        ucn$NCOL[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$NROW[[transport_step]][k] <- readBin(con,what='integer',n=1)
        ucn$ILAY[[transport_step]][k] <- readBin(con,what='integer',n=1)
      
      # Record 2
        ucn$CNEW[[transport_step]][,,k] <- matrix(readBin(con,what='numeric',n=btn$NCOL*btn$NROW,size=4),nrow=btn$NROW,ncol=btn$NCOL,byrow=TRUE)
    }
    class(ucn$CNEW[[transport_step]]) <- 'mt3dms_3d_array'
    ucn$CNEW[[transport_step]][which(ucn$CNEW[[transport_step]]==btn$CINACT)] <- NA
  }
  close(con)

  class(ucn) <- c('ucn','mt3dms_package')
  return(ucn)
}