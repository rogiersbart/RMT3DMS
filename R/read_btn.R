#' Read an MT3DMS basic transport package file
#' 
#' \code{read_btn} reads in an MT3DMS basic transport package file and returns it as an \code{\link{RMT3DMS}} btn object.
#' 
#' @param file filename; typically '*.btn'
#' @return object of class btn
#' @importFrom readr read_lines
#' @export
read_btn <- function(file)
{
  btn.lines <- read_lines(file)
  btn <- NULL
  
  # Data set A1
    btn$HEADNG[1] <- btn.lines[1]
    btn.lines <- btn.lines[-1]
  
  # Data set A2
    btn$HEADNG[2] <- btn.lines[1]
    btn.lines <- btn.lines[-1]
  
  # Data set A3
    dataSetA3 <- remove_empty_strings(strsplit(btn.lines[1],' ')[[1]])
    btn.lines <- btn.lines[-1]  
    btn$NLAY <- as.numeric(dataSetA3[1])
    btn$NROW <- as.numeric(dataSetA3[2])
    btn$NCOL <- as.numeric(dataSetA3[3])
    btn$NPER <- as.numeric(dataSetA3[4])
    btn$NCOMP <- as.numeric(dataSetA3[5])
    btn$MCOMP <- as.numeric(dataSetA3[6])
    rm(dataSetA3)
  
  # Data set A4
    dataSetA4 <- remove_empty_strings(strsplit(btn.lines[1],' ')[[1]])
    btn.lines <- btn.lines[-1]  
    btn$TUNIT <- dataSetA4[1]
    btn$LUNIT <- dataSetA4[2]
    btn$MUNIT <- dataSetA4[3]
    rm(dataSetA4)
  
  # Data set A5
    btn$TRNOP <- as.logical(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
    btn.lines <- btn.lines[-1]  
    
  # Data set A6
    btn$LAYCON <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
    btn.lines <- btn.lines[-1]
          
  # Data set A7
    dataSetA7 <- int_get_mt3dms_array(btn.lines,1,btn$NCOL,1)
    btn$DELR <- dataSetA7$mfarray
    btn.lines <- dataSetA7$remaining.lines
    rm(dataSetA7)
          
  # Data set A8
    dataSetA8 <- int_get_mt3dms_array(btn.lines,1,btn$NROW,1)
    btn$DELC <- dataSetA8$mfarray
    btn.lines <- dataSetA8$remaining.lines
    rm(dataSetA8)
          
  # Data set A9
    dataSetA9 <- int_get_mt3dms_array(btn.lines,btn$NROW,btn$NCOL,1)
    btn.lines <- dataSetA9$remaining.lines
    btn$HTOP <- dataSetA9$mfarray
    rm(dataSetA9)
          
  # Data set A10
    dataSetA10 <- int_get_mt3dms_array(btn.lines,btn$NROW,btn$NCOL,btn$NLAY)
    btn.lines <- dataSetA10$remaining.lines
    btn$DZ <- dataSetA10$mfarray
    rm(dataSetA10)
  
  # Data set A11
    dataSetA11 <- int_get_mt3dms_array(btn.lines,btn$NROW,btn$NCOL,btn$NLAY)
    btn.lines <- dataSetA11$remaining.lines
    btn$PRSITY <- dataSetA11$mfarray
    rm(dataSetA11)
  
  # Data set A12
    dataSetA12 <- int_get_mt3dms_array(btn.lines,btn$NROW,btn$NCOL,btn$NLAY)
    btn.lines <- dataSetA12$remaining.lines
    btn$ICBUND <- dataSetA12$mfarray
    rm(dataSetA12)
  
  # Data set A13
    btn$SCONC <- list()
    for(species in 1:btn$NCOMP)
    {
      dataSetA13 <- int_get_mt3dms_array(btn.lines,btn$NROW,btn$NCOL,btn$NLAY)
      btn.lines <- dataSetA13$remaining.lines
      btn$SCONC[[species]] <- dataSetA13$mfarray
      rm(dataSetA13)
    }
  
  # Data set A14
    dataSetA14 <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
    btn$CINACT <- dataSetA14[1]
    btn$THKMIN <- dataSetA14[2]
    btn.lines <- btn.lines[-1]
    rm(dataSetA14)
  
  # Data set A15
    dataSetA15 <- remove_empty_strings(strsplit(btn.lines[1],' ')[[1]])
    btn$IFMTCN <- as.numeric(dataSetA15[1])
    btn$IFMTNP <- as.numeric(dataSetA15[2])
    btn$IFMTRF <- as.numeric(dataSetA15[3])
    btn$IFMTDP <- as.numeric(dataSetA15[4])
    btn$SAVUCN <- as.logical(dataSetA15[5])
    btn.lines <- btn.lines[-1]    
    rm(dataSetA15)
  
  # Data set A16
    btn$NPRS <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
    btn.lines <- btn.lines[-1]
    
  # Data set A17
    if(btn$NPRS > 0)
    {
      nLines <- (btn$NPRS %/% 8 + ifelse((btn$NPRS %% 8)==0, 0, 1))
      btn$TIMPRS <- as.numeric(remove_empty_strings(strsplit0(paste(btn.lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]]))
      btn.lines <- btn.lines[-c(1:nLines)]
    }
  
  # Data set A18
    dataSetA18 <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
    btn$NOBS <- dataSetA18[1]
    btn$NPROBS <- dataSetA18[2]
    btn.lines <- btn.lines[-1]
    rm(dataSetA18)
  
  # Data set A19
    if(btn$NOBS > 0)
    {
      btn$KOBS <- NULL
      btn$IOBS <- NULL
      btn$JOBS <- NULL
      for(i in 1:btn$NOBS)
      {
        dataSetA19 <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
        btn$KOBS[i] <- dataSetA19[1]
        btn$IOBS[i] <- dataSetA19[2]
        btn$JOBS[i] <- dataSetA19[3]
        btn.lines <- btn.lines[-1]
      }
      rm(dataSetA19)
    }
  
  # Data set A20
    dataSetA20 <- remove_empty_strings(strsplit(btn.lines[1],' ')[[1]])
    btn$CHKMAS <- as.logical(dataSetA20[1])
    btn$NPRMAS <- as.numeric(dataSetA20[2])
    btn.lines <- btn.lines[-1]
    rm(dataSetA20)
  
  btn$PERLEN <- NULL
  btn$NSTP <- NULL
  btn$TSMULT <- NULL
  btn$TSLNGH <- list()
  btn$DT0 <- NULL
  btn$MXSTRN <- NULL
  btn$TTSMULT <- NULL
  btn$TTSMAX <- NULL
  
  for(i in 1:btn$NPER)
  {  
    # Data set A21
      dataSetA21 <- remove_empty_strings(strsplit(btn.lines[1],' ')[[1]])
      btn$PERLEN[i] <- as.numeric(dataSetA21[1])
      btn$NSTP[i] <- as.numeric(dataSetA21[2])
      btn$TSMULT[i] <- as.numeric(dataSetA21[3])
      if(btn$NPER==1)
      {
        btn$SSTATE <- ifelse(as.character(dataSetA21[4])=='SSTATE',TRUE,FALSE)
        if(is.na(btn$SSTATE)) btn$SSTATE <- FALSE
      }
      btn.lines <- btn.lines[-1]
      rm(dataSetA21)
  
    # Data set A22
      if(btn$TSMULT[i] <= 0)
      {
        nLines <- (btn$NSTP %/% 8 + ifelse((btn$NPRS %% 8)==0, 0, 1))
        btn$TSLNGH[[i]] <- as.numeric(remove_empty_strings(strsplit0(paste(btn.lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]]))
        btn.lines <- btn.lines[-c(1:nLines)]
      }
    
    # Data set A23
      dataSetA23 <- as.numeric(remove_empty_strings(strsplit(btn.lines[1],' ')[[1]]))
      btn$DT0[i] <- dataSetA23[1]
      btn$MXSTRN[i] <- dataSetA23[2]
      btn$TTSMULT[i] <- dataSetA23[3]
      btn$TTSMAX[i] <- dataSetA23[4]
      btn.lines <- btn.lines[-1]
      rm(dataSetA23)
  }
  class(btn) <- c('btn','mt3dms_package')
  return(btn)
}