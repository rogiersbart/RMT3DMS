#' Write an MT3DMS file
#' 
#' @export
write_dsp <- function(dsp, file, btn, IPRN=-1)
{
  if(dsp$MultiDiffusion)
  {
    cat('$ MultiDiffusion\n',file=file)
  }
  
  # Data set C1    
    for(i in 1:dim(dsp$AL)[3])
    {
      if(i==1 & !dsp$MultiDiffusion) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file)
      if(i==1 & dsp$MultiDiffusion) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      if(i!=1) cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(dsp$AL[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  
  # Data set C2
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(dsp$TRPT, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C3
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(dsp$TRPV, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
  
  # Data set C4
    if(dsp$MultiDiffusion)
    {
      for(comp in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(dsp$DMCOEF[[comp]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    } else {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      cat(paste(paste(dsp$DMCOEF, collapse=' '), '\n', sep=''), file=file, append=TRUE) 
    }
}