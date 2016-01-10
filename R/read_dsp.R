#' Read an MT3DMS dispersion package file
#' 
#' \code{read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file filename; typically '*.dsp'
#' @param btn basic transport package file object
#' @return object of class dsp
#' @importFrom readr read_lines
#' @export
read_dsp <- function(file = {cat('Please select dsp file ...\n'); file.choose()},
                     btn = read_btn()) {
  
  dsp.lines <- read_lines(file)
  dsp <- NULL
  
  # multidiffusion option
    multiDiffusionOption <- remove_empty_strings(strsplit(dsp.lines[1],' ')[[1]])
    if(multiDiffusionOption[2]=='MultiDiffusion')
    {
      dsp$multidiffusion <- TRUE
      dsp.lines <- dsp.lines[-1]
    }
  
  # Data set C1
    data_set_c1 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
    dsp$al <- data_set_c1$array
    dsp.lines <- data_set_c1$remaining_lines
    rm(data_set_c1)
 
  # Data set C2
    data_set_c2 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
    dsp$trpt <- data_set_c2$array
    dsp.lines <- data_set_c2$remaining_lines
    rm(data_set_c2)
  
  # Data set C3
    data_set_c3 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
    dsp$trpv <- data_set_c3$array
    dsp.lines <- data_set_c3$remaining_lines
    rm(data_set_c3)
  
  # Data set C4
    if(dsp$multidiffusion) {
      dsp$dmcoef <- list()
      for(comp in 1:btn$ncomp) {
        data_set_c4 <- read_mt3dms_array(dsp.lines,btn$nrow,btn$ncol,btn$nlay)
        dsp$dmcoef[[comp]] <- data_set_c4$array
        dsp.lines <- data_set_c4$remaining_lines
      }
      rm(data_set_c4)
    } else {
      data_set_c4 <- read_mt3dms_array(dsp.lines,1,btn$nlay,1)
      dsp$dmcoef <- data_set_c4$array
      dsp.lines <- data_set_c4$remaining_lines
      rm(data_set_c4)
    }

  class(dsp) <- c('dsp','mt3dms_package')
  return(dsp)
}
