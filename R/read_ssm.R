#' Read an MT3DMS sink & source mixing package file
#' 
#' \code{read.ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \code{\link{RMT3DMS}} ssm object.
#' 
#' @param file Filename; typically *.ssm
#' @return Object of class ssm
#' @importFrom readr read_lines
#' @export
read_ssm <- function(file, btn)
{
  ssm.lines <- read_lines(file)
  ssm <- NULL
  
  # Data set D1
    dataSetD1 <- as.logical(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
    ssm$FWEL <- dataSetD1[1]
    ssm$FDRN <- dataSetD1[2]
    ssm$FRCH <- dataSetD1[3]
    ssm$FEVT <- dataSetD1[4]
    ssm$FRIV <- dataSetD1[5]
    ssm$FGHB <- dataSetD1[6]
    ssm$FNEW <- dataSetD1[7:10]
    ssm.lines <- ssm.lines[-1]  
  
  # Data set D2
    ssm$MXSS <- as.numeric(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
    ssm.lines <- ssm.lines[-1]  
  
  
  ssm$INCRCH <- NULL
  ssm$CRCH <- list()
  ssm$INCEVT <- NULL
  ssm$CEVT <- list()
  ssm$NSS <- NULL
  ssm$KSS <- list()
  ssm$ISS <- list()
  ssm$JSS <- list()
  ssm$CSS <- list()
  ssm$ITYPE <- list()
  ssm$CSSMS <- list()
  
  for(stress_period in 1:btn$NPER)
  {
    # Data set D3
      if(ssm$FRCH)
      {
        ssm$INCRCH[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
        ssm.lines <- ssm.lines[-1] 
      
    
    # Data set D4
      if(ssm$FRCH & (ssm$INCRCH[stress_period] >= 0))
      {
        dataSetD4 <- int_get_mt3dms_array(ssm.lines,btn$NROW,btn$NCOL,btn$NCOMP)
        ssm.lines <- dataSetD4$remaining.lines
        ssm$CRCH[[stress_period]] <- dataSetD4$mfarray
        rm(dataSetD4)
      }}
    
    # Data set D5
      if(ssm$FEVT)
      {
        ssm$INCEVT[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
        ssm.lines <- ssm.lines[-1] 
      
    
    # Data set D6
      if(ssm$FEVT & (ssm$INCEVT[stress_period] >= 0))
      {
        dataSetD6 <- int_get_mt3dms_array(ssm.lines,btn$NROW,btn$NCOL,btn$NCOMP)
        ssm.lines <- dataSetD6$remaining.lines
        ssm$CEVT[[stress_period]] <- dataSetD6$mfarray
        rm(dataSetD6)
      }}
    
    # Data set D7
      ssm$NSS[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
      ssm.lines <- ssm.lines[-1] 
    
    ssm$KSS[[stress_period]] <- rep(NA,ssm$NSS[stress_period])
    ssm$ISS[[stress_period]] <- ssm$KSS
    ssm$JSS[[stress_period]] <- ssm$KSS
    ssm$CSS[[stress_period]] <- ssm$KSS
    ssm$ITYPE[[stress_period]] <- ssm$KSS
    ssm$CSSMS[[stress_period]] <- list()
    
    # Data set D8
      if(ssm$NSS > 0)
      {
        for(i in 1:ssm$NSS[stress_period])
        {
          dataSetD8 <- as.logical(remove_empty_strings(strsplit(ssm.lines[1],' ')[[1]]))
          ssm$KSS[[stress_period]][i] <- dataSetD8[1]
          ssm$ISS[[stress_period]][i] <- dataSetD8[2]
          ssm$JSS[[stress_period]][i] <- dataSetD8[3]
          ssm$CSS[[stress_period]][i] <- dataSetD8[4]
          ssm$ITYPE[[stress_period]][i] <- dataSetD8[5]
          ssm$CSSMS[[stress_period]][[i]] <- dataSetD8[6:(6+btn$NCOMP-1)]
          ssm.lines <- ssm.lines[-1] 
        }
      }
  }
  
  class(ssm) <- c('ssm','mt3dms_package')
  return(ssm)
}