#' Write an MT3DMS generalized conjugate gradient solver package file
#' 
#' @param gcg an \code{\link{RMT3DMS}} gcg object
#' @param file filename to write to; typically '*.gcg'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_gcg <- function(gcg, file, IPRN=-1)
{
  # Data set F1
    cat(paste0(c(prettyNum(c(gcg$mxiter,gcg$iter1,gcg$isolve,gcg$ncrs),width=10), '\n'),collapse=''), file=file)
  
  # Data set F2
    cat(paste0(c(formatC(c(gcg$accl,gcg$cclose,gcg$iprgcg),format='fg'), '\n'),collapse=' '), file=file, append=TRUE)
}
