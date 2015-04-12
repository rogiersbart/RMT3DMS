#' Write an MT3DMS file
#' 
#' @export
write_ssm <- function(ssm, file, btn, IPRN=-1)
{
  # Data set D1
    cat(paste0(c(as.character(factor(c(ssm$FWEL,ssm$FDRN,ssm$FRCH,ssm$FEVT,ssm$FRIV,ssm$FGHB,ssm$FNEW),levels=c(TRUE,FALSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file)
  
  # Data set D2
    cat(paste0(c(prettyNum(c(ssm$MXSS),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  for(stress_period in 1:btn$NPER)
  {
    # Data set D3
      if(ssm$FRCH)
      {
        cat(paste0(c(prettyNum(c(ssm$INCRCH[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
        # Data set D4
          if(ssm$FRCH & (ssm$INCRCH[stress_period] >= 0))
          {
            for(i in 1:btn$NCOMP)
            {
              cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
              if(btn$NCOMP>1) write.table(ssm$CRCH[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
              if(btn$NCOMP==1) write.table(ssm$CRCH[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
            }
          }
      }
    
    # Data set D5
      if(ssm$FEVT)
      {
        cat(paste0(c(prettyNum(c(ssm$INCEVT[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        
        # Data set D6
          if(ssm$FEVT & (ssm$INCEVT[stress_period] >= 0))
          {
            for(i in 1:btn$NCOMP)
            {
              cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
              if(btn$NCOMP>1) write.table(ssm$CEVT[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
              if(btn$NCOMP==1) write.table(ssm$CEVT[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
            }
          }
      }
    
    # Data set D7
      cat(paste0(c(prettyNum(c(ssm$NSS[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set D8
      if(ssm$NSS > 0)
      {
        for(i in 1:ssm$NSS[stress_period])
        {
          if(btn$NCOMP > 1) cat(paste0(c(prettyNum(c(ssm$KSS[[stress_period]][i],ssm$ISS[[stress_period]][i],ssm$JSS[[stress_period]][i],ssm$CSS[[stress_period]][i],ssm$ITYPE[[stress_period]][i],ssm$CSSMS[[stress_period]][[i]]),width=10), '\n'),collapse=''), file=file, append=TRUE)
          if(btn$NCOMP == 1) cat(paste0(c(prettyNum(c(ssm$KSS[[stress_period]][i],ssm$ISS[[stress_period]][i],ssm$JSS[[stress_period]][i],ssm$CSS[[stress_period]][i],ssm$ITYPE[[stress_period]][i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        }
      }
  }
}