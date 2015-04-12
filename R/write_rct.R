#' Write an MT3DMS file
#' 
#' @export
write_rct <- function(rct, file, btn, IPRN=-1)
{
  # Data set E1
    cat(paste0(c(prettyNum(c(rct$ISOTHM,rct$IREACT,rct$IRCTOP,rct$IGETSC),width=10), '\n'),collapse=''), file=file)
  
  # Data set E2A
    if(rct$ISOTHM %in% c(1,2,3,4,6))
    {
      for(i in 1:btn$NLAY)
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$RHOB[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  
  # Data set E2B
    if(rct$ISOTHM %in% c(5,6))
    {
      for(i in 1:btn$NLAY)
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$PRSITY2[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  
  # Data set E2C
    if(rct$IGETSC > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$SRCONC[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
  
  # Data set E3
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$SP1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
  
  # Data set E4
    if(rct$ISOTHM > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$SP2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
  
  # Data set E5
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$RC1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
  
  # Data set E6
    if(rct$IREACT > 0)
    {
      for(species in 1:btn$NCOMP)
      {
        for(i in 1:btn$NLAY)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$RC2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
}