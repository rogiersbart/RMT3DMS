#' Write an MT3DMS file
#' 
#' @param ssm an \code{\link{RMT3DMS}} ssm object
#' @param file filename to write to; typically '*.ssm'
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_ssm <- function(ssm, file, btn, IPRN=-1)
{
  # Data set D1
    cat(paste0(c(as.character(factor(c(ssm$fwel,ssm$fdrn,ssm$frch,ssm$fevt,ssm$friv,ssm$fghb,ssm$fnew),levels=c(TRUE,FalSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file)
  
  # Data set D2
    cat(paste0(c(prettyNum(c(ssm$mxss),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  for(stress_period in 1:btn$nper)
  {
    # Data set D3
      if(ssm$frch)
      {
        cat(paste0(c(prettyNum(c(ssm$incrch[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
        # Data set D4
          if(ssm$frch & (ssm$incrch[stress_period] >= 0))
          {
            for(i in 1:btn$ncomp)
            {
              cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
              if(btn$ncomp>1) write.table(ssm$crch[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
              if(btn$ncomp==1) write.table(ssm$crch[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
            }
          }
      }
    
    # Data set D5
      if(ssm$fevt)
      {
        cat(paste0(c(prettyNum(c(ssm$incevt[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        
        # Data set D6
          if(ssm$fevt & (ssm$incevt[stress_period] >= 0))
          {
            for(i in 1:btn$ncomp)
            {
              cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
              if(btn$ncomp>1) write.table(ssm$cevt[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
              if(btn$ncomp==1) write.table(ssm$cevt[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
            }
          }
      }
    
    # Data set D7
      cat(paste0(c(prettyNum(c(ssm$nss[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set D8
      if(ssm$nss > 0)
      {
        for(i in 1:ssm$nss[stress_period])
        {
          if(btn$ncomp > 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i],ssm$cssms[[stress_period]][[i]]),width=10), '\n'),collapse=''), file=file, append=TRUE)
          if(btn$ncomp == 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        }
      }
  }
}
