#' Read an MT3DMS generalized conjugate gradient solver package file
#' 
#' \code{read_gcg} reads in an MT3DMS generalized conjugate gradient solver package file and returns it as an \code{\link{RMT3DMS}} gcg object.
#' 
#' @param file filename; typically '*.gcg'
#' @return object of class gcg
#' @importFrom readr read_lines
#' @export
read_gcg <- function(file = {cat('Please select gcg file ...\n'); file.choose()}) {
  
  gcg_lines <- read_lines(file)
  gcg <- NULL
  
  # Data set F1
    data_set_f1 <- as.numeric(remove_empty_strings(strsplit(gcg_lines[1],' ')[[1]]))
    gcg$mxiter <- data_set_f1[1]
    gcg$iter1 <- data_set_f1[2]
    gcg$isolve <- data_set_f1[3]
    gcg$ncrs <- data_set_f1[4]
    gcg_lines <- gcg_lines[-1]  
  
  # Data set F2
    data_set_f2 <- as.numeric(remove_empty_strings(strsplit(gcg_lines[1],' ')[[1]]))
    gcg$accl <- data_set_f2[1]
    gcg$cclose <- data_set_f2[2]
    gcg$iprgcg <- data_set_f2[3]
    gcg_lines <- gcg_lines[-1]
    rm(data_set_f2)
  
  class(gcg) <- c('gcg','mt3dms_package')
  return(gcg)
}
