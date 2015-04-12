#' Write an MT3DMS file
#' 
#' @export
write_gcg <- function(gcg, file, IPRN=-1)
{
  # Data set F1
    cat(paste0(c(prettyNum(c(gcg$MXITER,gcg$ITER1,gcg$ISOLVE,gcg$NCRS),width=10), '\n'),collapse=''), file=file)
  
  # Data set F2
    cat(paste0(c(formatC(c(gcg$ACCL,gcg$CCLOSE,gcg$IPRGCG),format='fg'), '\n'),collapse=' '), file=file, append=TRUE)
}