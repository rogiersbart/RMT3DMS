#' Read an MT3DMS dispersion package file
#' 
#' \code{read.dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file Filename; typically *.dsp
#' @return Object of class dsp
#' @export
read.dsp <- function(file, btn)
{
  dsp.lines <- scan(file, what=character(), sep='\n')
  dsp <- NULL
  
  # MultiDiffusion option
    multiDiffusionOption <- remove.empty.strings(strsplit(dsp.lines[1],' ')[[1]])
    if(multiDiffusionOption[2]=='MultiDiffusion')
    {
      dsp$MultiDiffusion <- TRUE
      dsp.lines <- dsp.lines[-1]
    }
  
  # Data set C1
    dataSetC1 <- get_mt3dms_array(dsp.lines,btn$NROW,btn$NCOL,btn$NLAY)
    dsp$AL <- dataSetC1$mfarray
    dsp.lines <- dataSetC1$remaining.lines
    rm(dataSetC1)
 
  # Data set C2
    dataSetC2 <- get_mt3dms_array(dsp.lines,1,btn$NLAY,1)
    dsp$TRPT <- dataSetC2$mfarray
    dsp.lines <- dataSetC2$remaining.lines
    rm(dataSetC2)
  
  # Data set C3
    dataSetC3 <- get_mt3dms_array(dsp.lines,1,btn$NLAY,1)
    dsp$TRPV <- dataSetC3$mfarray
    dsp.lines <- dataSetC3$remaining.lines
    rm(dataSetC3)
  
  # Data set C4
    if(dsp$MultiDiffusion)
    {
      dsp$DMCOEF <- list()
      for(comp in 1:btn$NCOMP)
      {
        dataSetC4 <- get_mt3dms_array(dsp.lines,btn$NROW,btn$NCOL,btn$NLAY)
        dsp$DMCOEF[[comp]] <- dataSetC4$mfarray
        dsp.lines <- dataSetC4$remaining.lines
      }
      rm(dataSetC4)
    } else {
      dataSetC4 <- get_mt3dms_array(dsp.lines,1,btn$LAY,1)
      dsp$DMCOEF <- dataSetC4$mfarray
      dsp.lines <- dataSetC4$remaining.lines
      rm(dataSetC4)
    }

  class(dsp) <- c('dsp','mt3dms_package')
  return(dsp)
}