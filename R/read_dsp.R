#' Read an MT3DMS dispersion package file
#' 
#' \code{read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file filename; typically '*.dsp'
#' @param btn basic transport package file object
#' @return object of class dsp
#' @importFrom readr read_lines
#' @export
read_dsp <- function(file, btn)
{
  dsp.lines <- read_lines(file)
  dsp <- NULL
  
  # MultiDiffusion option
    multiDiffusionOption <- remove_empty_strings(strsplit(dsp.lines[1],' ')[[1]])
    if(multiDiffusionOption[2]=='MultiDiffusion')
    {
      dsp$MultiDiffusion <- TRUE
      dsp.lines <- dsp.lines[-1]
    }
  
  # Data set C1
    dataSetC1 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
    dsp$AL <- dataSetC1$array
    dsp.lines <- dataSetC1$remaining_lines
    rm(dataSetC1)
 
  # Data set C2
    dataSetC2 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
    dsp$TRPT <- dataSetC2$array
    dsp.lines <- dataSetC2$remaining_lines
    rm(dataSetC2)
  
  # Data set C3
    dataSetC3 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
    dsp$TRPV <- dataSetC3$array
    dsp.lines <- dataSetC3$remaining_lines
    rm(dataSetC3)
  
  # Data set C4
    if(dsp$MultiDiffusion)
    {
      dsp$DMCOEF <- list()
      for(comp in 1:btn$ncomp)
      {
        dataSetC4 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
        dsp$DMCOEF[[comp]] <- dataSetC4$array
        dsp.lines <- dataSetC4$remaining_lines
      }
      rm(dataSetC4)
    } else {
      dataSetC4 <- read_mt3dms_array(dsp.lines,1,btn$LAY,1)
      dsp$DMCOEF <- dataSetC4$array
      dsp.lines <- dataSetC4$remaining_lines
      rm(dataSetC4)
    }

  class(dsp) <- c('dsp','mt3dms_package')
  return(dsp)
}
