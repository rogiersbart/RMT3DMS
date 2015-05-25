#' Write an MT3DMS file
#' 
#' @export
write_btn <- function(btn, file, IPRN=-1)
{
  # Data set A1
    cat(paste(btn$HEADNG[1], '\n'), file=file, append=TRUE)
  
  # Data set A2
    cat(paste(btn$HEADNG[2], '\n'), file=file, append=TRUE)
  
  # Data set A3
    cat(paste0(c(prettyNum(c(btn$NLAY, btn$NROW, btn$NCOL, btn$NPER, btn$NCOMP, btn$MCOMP),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A4
    cat(paste0(c(prettyChar(c(btn$TUNIT, btn$LUNIT, btn$MUNIT),width=4), '\n'),collapse=''), file=file, append=TRUE)
   
  # Data set A5
    cat(paste0(c(as.character(factor(btn$TRNOP,levels=c(TRUE,FALSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A6
    cat(paste0(c(prettyNum(c(btn$LAYCON),width=2), '\n'),collapse=''), file=file, append=TRUE) 
  
  # Data set A7
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(btn$DELR, collapse=' '), '\n', sep=' '), file=file, append=TRUE) 
  
  # Data set A8
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    cat(paste(paste(btn$DELC, collapse=' '), '\n', sep=' '), file=file, append=TRUE) 
  
  # Data set A9
    cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
    write.table(btn$HTOP, file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)   
  
  # Data set A10
    for(i in 1:dim(btn$DZ)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$DZ[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  
  # Data set A11
    for(i in 1:dim(btn$PRSITY)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$PRSITY[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  
  # Data set A12
    for(i in 1:dim(btn$ICBUND)[3])
    {
      cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(btn$ICBUND[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  
  # Data set A13
    for(species in 1:btn$NCOMP)
    {
      btn$SCONC[[species]][which(is.na(btn$SCONC[[species]]))] <- btn$CINACT
      for(i in 1:dim(btn$SCONC[[species]])[3])
      {
        cat(paste('       103         1           (NOTUSED)', formatC(IPRN,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(btn$SCONC[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  
  # Data set A14
    cat(paste0(c(prettyNum(c(btn$CINACT,btn$THKMIN),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A15
    cat(paste0(c(prettyNum(c(btn$IFMTCN,btn$IFMTNP,btn$IFMTRF,btn$IFMTDP),width=10),ifelse(btn$SAVUCN,'         T','         F'), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A16
    cat(paste0(c(prettyNum(c(btn$NPRS),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A17
    if(btn$NPRS > 0)
    {
      nLines <- (btn$NPRS %/% 8 + ifelse((btn$NPRS %% 8)==0, 0, 1))
      for(i in 1:nLines)
      {
        cat(paste0(c(prettyNum(c(btn$TIMPRS[((i-1)*8+1):ifelse((i*8)>btn$NPRS,btn$NPRS,(i*8))]),width=10),'\n'),collapse=''),file=file,append=TRUE)
      }
    }

  # Data set A18
    cat(paste0(c(prettyNum(ifelse(is.na(btn$NPROBS),btn$NOBS,c(btn$NOBS,btn$NPROBS)),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  # Data set A19
    if(btn$NOBS > 0)
    {
      for(i in 1:btn$NOBS)
      {
        cat(paste0(c(prettyNum(c(btn$KOBS[i],btn$IOBS[i],btn$JOBS[i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      }
    }
  
  # Data set A20
    cat(paste0(c(ifelse(btn$CHKMAS,'         T','         F'),prettyNum(c(btn$NPRMAS),width=10), '\n'),collapse=''), file=file, append=TRUE)

  for(i in 1:btn$NPER)
  {  
    # Data set A21
      cat(paste0(c(prettyNum(c(btn$PERLEN[i],btn$NSTP[i],btn$TSMULT[i]),width=10),ifelse(btn$SSTATE,'    SSTATE',''), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set A22
      if(btn$TSMULT[i] <= 0)
      {
        nLines <- (btn$NSTP %/% 8 + ifelse((btn$NSTP %% 8)==0, 0, 1))
        for(i in 1:nLines)
        {
          cat(paste0(c(prettyNum(c(btn$TSLNGH[((i-1)*8+1):ifelse((i*8)>btn$NSTP,btn$NSTP,(i*8))]),width=10),'\n'),collapse=''))
        }
      }
    
    # Data set A23
      cat(paste0(c(formatC(c(btn$DT0[i]),width=10),formatC(btn$MXSTRN[i],width=10,format='d'),formatC(c(btn$TTSMULT[i],btn$TTSMAX[i]),width=10,format='fg'), '\n'),collapse=''), file=file, append=TRUE)
  }
}