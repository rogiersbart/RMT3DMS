#' Read an MT3DMS generalized conjugate gradient solver package file
#' 
#' \code{rmt_read_gcg} reads in an MT3DMS generalized conjugate gradient solver package file and returns it as an \code{\link{RMT3DMS}} gcg object.
#' 
#' @param file filename; typically '*.gcg'
#' @return object of class gcg
#' @export
rmt_read_gcg <- function(file = {cat('Please select gcg file ...\n'); file.choose()}) {
  
  gcg_lines <- readr::read_lines(file)
  gcg <- list()
  
  # Data set 1
  data_set_1 <- rmti_parse_variables(gcg_lines, format = 'free')
  gcg$mxiter <- as.numeric(data_set_1$variables[1])
  gcg$iter1 <- as.numeric(data_set_1$variables[2])
  gcg$isolve <- as.numeric(data_set_1$variables[3])
  gcg$ncrs <- as.numeric(data_set_1$variables[4])
  gcg_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2
  data_set_2 <- rmti_parse_variables(gcg_lines, format = 'free')
  gcg$accl <- as.numeric(data_set_2$variables[1])
  gcg$cclose <- as.numeric(data_set_2$variables[2])
  gcg$iprgcg <- as.numeric(data_set_2$variables[3])
  gcg_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  class(gcg) <- c('gcg','rmt_package')
  return(gcg)
}

#' Write an MT3DMS generalized conjugate gradient solver package file
#' 
#' @param gcg an \code{\link{RMT3DMS}} gcg object
#' @param file filename to write to; typically '*.gcg'
#' @return \code{NULL}
#' @export
rmt_write_gcg <- function(gcg,
                      file = {cat('Please select gcg file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # Data set 1
  rmti_write_variables(gcg$mxiter, gcg$iter1, gcg$isolve, gcg$ncrs, file = file, append = FALSE)
  
  # Data set 2
  rmti_write_variables(gcg$accl, gcg$cclose, gcg$iprgcg, file = file)
}
