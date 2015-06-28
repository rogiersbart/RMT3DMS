#' Read an MT3DMS generalized conjugate gradient solver package file
#' 
#' \code{read_gcg} reads in an MT3DMS generalized conjugate gradient solver package file and returns it as an \code{\link{RMT3DMS}} gcg object.
#' 
#' @param file filename; typically '*.gcg'
#' @return object of class gcg
#' @importFrom readr read_lines
#' @export
read_gcg <- function(file)
{
  gcg.lines <- read_lines(file)
  gcg <- NULL
  
  # Data set F1
    dataSetF1 <- as.numeric(remove_empty_strings(strsplit(gcg.lines[1],' ')[[1]]))
    gcg$MXITER <- dataSetF1[1]
    gcg$ITER1 <- dataSetF1[2]
    gcg$ISOLVE <- dataSetF1[3]
    gcg$NCRS <- dataSetF1[4]
    gcg.lines <- gcg.lines[-1]  
  
  # Data set F2
    dataSetF2 <- as.numeric(remove_empty_strings(strsplit(gcg.lines[1],' ')[[1]]))
    gcg$ACCL <- dataSetF2[1]
    gcg$CCLOSE <- dataSetF2[2]
    gcg$IPRGCG <- dataSetF2[3]
    gcg.lines <- gcg.lines[-1]
    rm(dataSetF2)
  
  class(gcg) <- c('gcg','mt3dms_package')
  return(gcg)
}