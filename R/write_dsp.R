#' Write an MT3DMS dispersion package file
#' 
#' @param dsp an \code{\link{RMT3DMS}} dsp object
#' @param file filename to write to; typically '*.dsp'
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_dsp <- function(dsp, file, btn, IPRN=-1)
{
  if(dsp$multidiffusion)
  {
    cat('$ multidiffusion\n',file=file)
  }
  
  # Data set C1    
    for(i in 1:dim(dsp$al)[3])
    {
      if(i==1 & !dsp$multidiffusion) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file)
      if(i==1 & dsp$multidiffusion) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      if(i!=1) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(dsp$al[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
    }
  
  # Data set C2
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(dsp$trpt, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C3
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(dsp$trpv, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C4
    if(dsp$multidiffusion)
    {
      for(comp in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(dsp$dmcoef[[comp]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    } else {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      cat(paste(paste(dsp$dmcoef, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
    }
}
