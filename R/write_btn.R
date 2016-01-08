#' Write an MT3DMS basic transport package file
#' 
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param file filename to write to; typically '*.btn'
#' @param IPRN format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
write_btn <- function(btn, file, IPRN=-1)
{
  # Data set A1
    cat(paste(btn$headng[1], '\n'), file=file, append=FalSE)
  
  # Data set A2
    cat(paste(btn$headng[2], '\n'), file=file, append=TRUE)
  
  # Data set A3
    cat(paste0(c(prettyNum(c(btn$nlay, btn$nrow, btn$ncol, btn$nper, btn$ncomp, btn$mcomp),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A4
    cat(paste0(c(prettyChar(c(btn$tunit, btn$lunit, btn$munit),width=4), '\n'),collapse=''), file=file, append=TRUE)
   
  # Data set A5
    cat(paste0(c(as.character(factor(btn$trnop,levels=c(TRUE,FalSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A6
    cat(paste0(c(prettyNum(c(btn$laycon),width=2), '\n'),collapse=''), file=file, append=TRUE) 
  
  # Data set A7
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(btn$delr, collapse=' '), '\n', sep=' '), file=file, append=TRUE) 
  
  # Data set A8
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(btn$delc, collapse=' '), '\n', sep=' '), file=file, append=TRUE) 
  
  # Data set A9
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    write.table(btn$htop, file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)   
  
  # Data set A10
    for(i in 1:dim(btn$dz)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$dz[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
    }
  
  # Data set A11
    for(i in 1:dim(btn$prsity)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$prsity[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
    }
  
  # Data set A12
    for(i in 1:dim(btn$icbund)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$icbund[,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
    }
  
  # Data set A13
    for(species in 1:btn$ncomp)
    {
      btn$sconc[[species]][which(is.na(btn$sconc[[species]]))] <- btn$cinact
      for(i in 1:dim(btn$sconc[[species]])[3])
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(btn$sconc[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FalSE, row.names=FalSE)       
      }
    }
  
  # Data set A14
    cat(paste0(c(prettyNum(c(btn$cinact,btn$thkmin),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A15
    cat(paste0(c(prettyNum(c(btn$ifmtcn,btn$ifmtnp,btn$ifmtrf,btn$ifmtdp),width=10),ifelse(btn$savucn,'         T','         F'), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A16
    cat(paste0(c(prettyNum(c(btn$nprs),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A17
    if(btn$nprs > 0)
    {
      nLines <- (btn$nprs %/% 8 + ifelse((btn$nprs %% 8)==0, 0, 1))
      for(i in 1:nLines)
      {
        cat(paste0(c(prettyNum(c(btn$timprs[((i-1)*8+1):ifelse((i*8)>btn$nprs,btn$nprs,(i*8))]),width=10),'\n'),collapse=''),file=file,append=TRUE)
      }
    }

  # Data set A18
    cat(paste0(c(prettyNum(ifelse(is.na(btn$nprobs),btn$nobs,c(btn$nobs,btn$nprobs)),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A19
    if(btn$nobs > 0)
    {
      for(i in 1:btn$nobs)
      {
        cat(paste0(c(prettyNum(c(btn$kobs[i],btn$iobs[i],btn$jobs[i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      }
    }
  
  # Data set A20
    cat(paste0(c(ifelse(btn$chkmas,'         T','         F'),prettyNum(c(btn$nprmas),width=10), '\n'),collapse=''), file=file, append=TRUE)

  for(i in 1:btn$nper)
  {  
    # Data set A21
      cat(paste0(c(prettyNum(c(btn$perlen[i],btn$nstp[i],btn$tsmult[i]),width=10),ifelse(btn$sstate,'    SSTATE',''), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set A22
      if(btn$tsmult[i] <= 0)
      {
        nLines <- (btn$nstp %/% 8 + ifelse((btn$nstp %% 8)==0, 0, 1))
        for(i in 1:nLines)
        {
          cat(paste0(c(prettyNum(c(btn$tslngh[((i-1)*8+1):ifelse((i*8)>btn$nstp,btn$nstp,(i*8))]),width=10),'\n'),collapse=''))
        }
      }
    
    # Data set A23
      cat(paste0(c(formatC(c(btn$dt0[i]),width=10),formatC(btn$mxstrn[i],width=10,format='d'),formatC(c(btn$ttsmult[i],btn$ttsmax[i]),width=10,format='fg'), '\n'),collapse=''), file=file, append=TRUE)
  }
}
