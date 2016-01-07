#' Read an MT3DMS chemical reaction package file
#' 
#' \code{read_rct} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} rct object.
#' 
#' @param file filename; typically '*.rct'
#' @param btn basic transport package file object
#' @return object of class rct
#' @importFrom readr read_lines
#' @export
read_rct <- function(file, btn)
{
  rct.lines <- read_lines(file)
  rct <- NULL
  
  # Data set E1
    dataSetE1 <- as.numerical(remove_empty_strings(strsplit(rct.lines[1],' ')[[1]]))
    rct$ISOTHM <- dataSetE1[1]
    rct$IREACT <- dataSetE1[2]
    rct$IRCTOP <- dataSetE1[3]
    rct$IGETSC <- dataSetE1[4]
    rct.lines <- rct.lines[-1]  
  
  # Data set E2A
    if(rct$ISOTHM %in% c(1,2,3,4,6))
    {
      dataSetE2A <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
      rct.lines <- dataSetE2A$remaining_lines
      rct$RHOB <- dataSetE2A$array
      rm(dataSetE2A)
    }
  
  # Data set E2B
    if(rct$ISOTHM %in% c(5,6))
    {
      dataSetE2B <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
      rct.lines <- dataSetE2B$remaining_lines
      rct$prsity2 <- dataSetE2B$array
      rm(dataSetE2B)
    }
  
  # Data set E2C
    rct$SRCONC <- list()
    if(rct$IGETSC > 0)
    {
      for(species in 1:btn$ncomp)
      {
        dataSetE2C <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
        rct.lines <- dataSetE2C$remaining_lines
        rct$SRCONC[[species]] <- dataSetE2C$array
        rm(dataSetE2C)
      }
    }
  
  # Data set E3
    rct$SP1 <- list()
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$ncomp)
      {
        dataSetE3 <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
        rct.lines <- dataSetE3$remaining_lines
        rct$SP1[[species]] <- dataSetE3$array
        rm(dataSetE3)
      }
    }
  
  # Data set E4
    rct$SP2 <- list()
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$ncomp)
      {
        dataSetE4 <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
        rct.lines <- dataSetE4$remaining_lines
        rct$SP2[[species]] <- dataSetE4$array
        rm(dataSetE4)
      }
    }
  
  # Data set E5
    rct$RC1 <- list()
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$ncomp)
      {
        dataSetE5 <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
        rct.lines <- dataSetE5$remaining_lines
        rct$RC1[[species]] <- dataSetE5$array
        rm(dataSetE5)
      }
    }
  
  # Data set E6
    rct$RC2 <- list()
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$ncomp)
      {
        dataSetE6 <- read_mt3dms_array(rct.lines,btn$nrow,btn$ncol,btn$nlay)
        rct.lines <- dataSetE6$remaining_lines
        rct$RC2[[species]] <- dataSetE6$array
        rm(dataSetE6)
      }
    }
  
  class(rct) <- c('rct','mt3dms_package')
  return(rct)
}
