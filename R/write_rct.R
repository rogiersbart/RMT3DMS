#' Write an MT3DMS file
#' 
#' @param rct an \code{\link{RMT3DMS}} rct object
#' @param file filename to write to; typically '*.rct'
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_rct <- function(rct, file, btn, IPRN=-1)
{
  # Data set E1
    cat(paste0(c(prettyNum(c(rct$isothm,rct$ireact,rct$irctop,rct$igetsc),width=10), '\n'),collapse=''), file=file)
  
  # Data set E2A
    if(rct$isothm %in% c(1,2,3,4,6))
    {
      for(i in 1:btn$nlay)
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$rhob[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
      }
    }
  
  # Data set E2B
    if(rct$isothm %in% c(5,6))
    {
      for(i in 1:btn$nlay)
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$prsity2[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
      }
    }
  
  # Data set E2C
    if(rct$igetsc > 0)
    {
      for(species in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$srconc[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    }
  
  # Data set E3
    if(rct$isothm > 0)
    {
      for(species in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$sp1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    }
  
  # Data set E4
    if(rct$isothm > 0)
    {
      for(species in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$sp2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    }
  
  # Data set E5
    if(rct$ireact > 0)
    {
      for(species in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$rc1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    }
  
  # Data set E6
    if(rct$ireact > 0)
    {
      for(species in 1:btn$ncomp)
      {
        for(i in 1:btn$nlay)
        {
          cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
          write.table(rct$rc2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
        }
      }
    }
}
