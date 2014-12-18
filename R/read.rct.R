#' Read an MT3DMS chemical reaction package file
#' 
#' \code{read.rct} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} rct object.
#' 
#' @param file Filename; typically *.rct
#' @return Object of class rct
#' @export
read.rct <- function(file, btn)
{
  rct.lines <- scan(file, what=character(), sep='\n')
  rct <- NULL
  
  # Data set E1
    dataSetE1 <- as.numerical(remove.empty.strings(strsplit(rct.lines[1],' ')[[1]]))
    rct$ISOTHM <- dataSetE1[1]
    rct$IREACT <- dataSetE1[2]
    rct$IRCTOP <- dataSetE1[3]
    rct$IGETSC <- dataSetE1[4]
    rct.lines <- rct.lines[-1]  
  
  # Data set E2A
    if(rct$ISOTHM %in% c(1,2,3,4,6))
    {
      dataSetE2A <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
      rct.lines <- dataSetE2A$remaining.lines
      rct$RHOB <- dataSetE2A$mfarray
      rm(dataSetE2A)
    }
  
  # Data set E2B
    if(rct$ISOTHM %in% c(5,6))
    {
      dataSetE2B <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
      rct.lines <- dataSetE2B$remaining.lines
      rct$PRSITY2 <- dataSetE2B$mfarray
      rm(dataSetE2B)
    }
  
  # Data set E2C
    rct$SRCONC <- list()
    if(rct$IGETSC > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        dataSetE2C <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
        rct.lines <- dataSetE2C$remaining.lines
        rct$SRCONC[[species]] <- dataSetE2C$mfarray
        rm(dataSetE2C)
      }
    }
  
  # Data set E3
    rct$SP1 <- list()
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        dataSetE3 <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
        rct.lines <- dataSetE3$remaining.lines
        rct$SP1[[species]] <- dataSetE3$mfarray
        rm(dataSetE3)
      }
    }
  
  # Data set E4
    rct$SP2 <- list()
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        dataSetE4 <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
        rct.lines <- dataSetE4$remaining.lines
        rct$SP2[[species]] <- dataSetE4$mfarray
        rm(dataSetE4)
      }
    }
  
  # Data set E5
    rct$RC1 <- list()
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        dataSetE5 <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
        rct.lines <- dataSetE5$remaining.lines
        rct$RC1[[species]] <- dataSetE5$mfarray
        rm(dataSetE5)
      }
    }
  
  # Data set E6
    rct$RC2 <- list()
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        dataSetE6 <- get_mt3dms_array(rct.lines,btn$NROW,btn$NCOL,btn$NLAY)
        rct.lines <- dataSetE6$remaining.lines
        rct$RC2[[species]] <- dataSetE6$mfarray
        rm(dataSetE6)
      }
    }
  
  class(rct) <- c('rct','mt3dms_package')
  return(rct)
}