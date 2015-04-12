#' Read an MT3DMS advection package file
#' 
#' \code{read.adv} reads in an MT3DMS advection package file and returns it as an \code{\link{RMT3DMS}} adv object.
#' 
#' @param file Filename; typically *.adv
#' @return Object of class adv
#' @export
read_adv <- function(file)
{
  adv.lines <- scan(file, what=character(), sep='\n')
  adv <- NULL
 
  # Data set B1
    dataSetB1 <- remove_empty_strings(strsplit(adv.lines[1],' ')[[1]])
    adv.lines <- adv.lines[-1]  
    adv$MIXELM <- as.numeric(dataSetB1[1])
    adv$PERCEL <- as.numeric(dataSetB1[2])
    adv$MXPART <- as.numeric(dataSetB1[3])
    adv$NADVFD <- as.numeric(dataSetB1[4])
    rm(dataSetB1)
  
  # Data set B2
    if(adv$MIXELM %in% c(1,2,3))
    {  
      dataSetB2 <- remove_empty_strings(strsplit(adv.lines[1],' ')[[1]])
      adv.lines <- adv.lines[-1]  
      adv$ITRACK <- as.numeric(dataSetB2[1])
      adv$WD <- as.numeric(dataSetB2[2])
      rm(dataSetB2)
    }
  
  # Data set B3
    if(adv$MIXELM %in% c(1,3))
    {  
      dataSetB3 <- remove_empty_strings(strsplit(adv.lines[1],' ')[[1]])
      adv.lines <- adv.lines[-1]  
      adv$DCEPS <- as.numeric(dataSetB3[1])
      adv$NPLANE <- as.numeric(dataSetB3[2])
      adv$NPL <- as.numeric(dataSetB3[3])
      adv$NPH <- as.numeric(dataSetB3[4])
      adv$NPMIN <- as.numeric(dataSetB3[5])
      adv$NPMAX <- as.numeric(dataSetB3[6])
      rm(dataSetB3)
    }
  
  # Data set B4
    if(adv$MIXELM %in% c(2,3))
    {  
      dataSetB4 <- remove_empty_strings(strsplit(adv.lines[1],' ')[[1]])
      adv.lines <- adv.lines[-1]  
      adv$INTERP <- as.numeric(dataSetB4[1])
      adv$NLSINK <- as.numeric(dataSetB4[2])
      adv$NPSINK <- as.numeric(dataSetB4[3])
      rm(dataSetB4)
    }
  
  # Data set B5
    if(adv$MIXELM==3)
    {  
      adv$DCHMOC <- as.numeric(remove_empty_strings(strsplit(adv.lines[1],' ')[[1]]))
      adv.lines <- adv.lines[-1]  
    }
  
  class(adv) <- c('adv','mt3dms_package')
  return(adv)
}