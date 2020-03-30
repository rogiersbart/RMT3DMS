
#' Create an \code{RMT3DMS} gcg object
#'
#' \code{rmt_create_gcg} creates an \code{RMT3DMS} dsp object.
#'
#' @param mxiter maximum number of outer iterations. See details. Defaults to 1.
#' @param iter1 maximum number of inner iterations. Defaults to 50.
#' @param isolve type of preconditioner: 1 = Jacobi, 2 = SSOR, 3 = Modified Incomplete Cholesky (MIC; default).
#' @param ncrs integer controlling treatment of dispersion tensor cross-terms. If 0 (default), lump all dispersion cross-terms to RHS. If 1, includes full dispersion tensor.
#' @param accl relaxtion factor for the SSOR preconditioner. Defaults to 1.
#' @param cclose Convergence criteration in relative concentration. Defaults to 1e-6.
#' @param iprgcg interval for printing maximum concentration. changes of each iteration. Defaults to 0 (only print at end of stress-period).
#'
#' @details \code{mxiter} should be higher than 1 when nonlinear sorption isotherm is included or when \code{drycell = TRUE} is used in \code{btn}.
#'
#' @return an object of class \code{gcg}
#' @export
#' @seealso \code{\link{rmt_read_gcg}}, \code{\link{rmt_write_gcg}}
#' @examples
#' rmt_create_gcg()
#' rmt_create_gcg(mxiter = 20, ncrs = 1, cclose = 1e-5, iprgcg = 1)
rmt_create_gcg <- function(mxiter = 1,
                           iter1 = 50,
                           isolve = 3,
                           ncrs = 0,
                           accl = 1,
                           cclose = 1e-6,
                           iprgcg = 0) {
  
  gcg <- list()
  
  # data set 1
  gcg$mxiter <- mxiter
  gcg$iter1 <- iter1
  gcg$isolve <- isolve
  gcg$ncrs <- ncrs
  
  # data set 2
  gcg$accl <- accl
  gcg$cclose <- cclose
  gcg$iprgcg <- iprgcg
  
  class(gcg) <- c('gcg', 'rmt_package')
  return(gcg)
  
}

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
#' @param gcg an \code{RMT3DMS} gcg object
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
