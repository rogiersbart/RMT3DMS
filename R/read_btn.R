#' Read an MT3DMS basic transport package file
#' 
#' \code{read_btn} reads in an MT3DMS basic transport package file and returns it as an \code{\link{RMT3DMS}} btn object.
#' 
#' @param file filename; typically '*.btn'
#' @return object of class btn
#' @importFrom readr read_lines
#' @export
read_btn <- function(file = {cat('Please select btn file ...\n'); file.choose()}) {
  
  btn_lines <- read_lines(file)
  btn <- NULL
  
  # Data set A1
    btn$headng[1] <- btn_lines[1]
    btn_lines <- btn_lines[-1]
  
  # Data set A2
    btn$headng[2] <- btn_lines[1]
    btn_lines <- btn_lines[-1]
  
  # Data set A3
    data_set_a3 <- read_mt3dms_variables(btn_lines)
    btn$nlay <- as.numeric(data_set_a3$variables[1])
    btn$nrow <- as.numeric(data_set_a3$variables[2])
    btn$ncol <- as.numeric(data_set_a3$variables[3])
    btn$nper <- as.numeric(data_set_a3$variables[4])
    btn$ncomp <- as.numeric(data_set_a3$variables[5])
    btn$mcomp <- as.numeric(data_set_a3$variables[6])
    btn_lines <- data_set_a3$remaining_lines
    rm(data_set_a3)
  
  # Data set A4
    data_set_a4 <- remove_empty_strings(strsplit(btn_lines[1],' ')[[1]])
    btn_lines <- btn_lines[-1]  
    btn$tunit <- data_set_a4[1]
    btn$lunit <- data_set_a4[2]
    btn$munit <- data_set_a4[3]
    rm(data_set_a4)
  
  # Data set A5
    btn$trnop <- as.logical(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
    btn_lines <- btn_lines[-1]  
    
  # Data set A6
    btn$laycon <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
    btn_lines <- btn_lines[-1]
          
  # Data set A7
    data_set_a7 <- read_mt3dms_array(btn_lines,1,btn$ncol,1, ndim = 1)
    btn$delr <- data_set_a7$array
    btn_lines <- data_set_a7$remaining_lines
    rm(data_set_a7)
          
  # Data set A8
    data_set_a8 <- read_mt3dms_array(btn_lines,1,btn$nrow,1, ndim = 1)
    btn$delc <- data_set_a8$array
    btn_lines <- data_set_a8$remaining_lines
    rm(data_set_a8)
          
  # Data set A9
    data_set_a9 <- read_mt3dms_array(btn_lines,btn$nrow,btn$ncol,1)
    btn_lines <- data_set_a9$remaining_lines
    btn$htop <- data_set_a9$array
    rm(data_set_a9)
          
  # Data set A10
    data_set_a10 <- read_mt3dms_array(btn_lines,btn$nrow,btn$ncol,btn$nlay)
    btn_lines <- data_set_a10$remaining_lines
    btn$dz <- data_set_a10$array
    rm(data_set_a10)
  
  # Data set A11
    data_set_a11 <- read_mt3dms_array(btn_lines,btn$nrow,btn$ncol,btn$nlay)
    btn_lines <- data_set_a11$remaining_lines
    btn$prsity <- data_set_a11$array
    rm(data_set_a11)
  
  # Data set A12
    data_set_a12 <- read_mt3dms_array(btn_lines,btn$nrow,btn$ncol,btn$nlay)
    btn_lines <- data_set_a12$remaining_lines
    btn$icbund <- data_set_a12$array
    rm(data_set_a12)
  
  # Data set A13
    btn$sconc <- list()
    for(species in 1:btn$ncomp) {
      data_set_a13 <- read_mt3dms_array(btn_lines,btn$nrow,btn$ncol,btn$nlay)
      btn_lines <- data_set_a13$remaining_lines
      btn$sconc[[species]] <- data_set_a13$array
      rm(data_set_a13)
    }
  
  # Data set A14
    data_set_a14 <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
    btn$cinact <- data_set_a14[1]
    btn$thkmin <- data_set_a14[2]
    btn_lines <- btn_lines[-1]
    rm(data_set_a14)
  
  # Data set A15
    data_set_a15 <- remove_empty_strings(strsplit(btn_lines[1],' ')[[1]])
    btn$ifmtcn <- as.numeric(data_set_a15[1])
    btn$ifmtnp <- as.numeric(data_set_a15[2])
    btn$ifmtrf <- as.numeric(data_set_a15[3])
    btn$ifmtdp <- as.numeric(data_set_a15[4])
    btn$savucn <- as.logical(data_set_a15[5])
    btn_lines <- btn_lines[-1]    
    rm(data_set_a15)
  
  # Data set A16
    btn$nprs <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
    btn_lines <- btn_lines[-1]
    
  # Data set A17
    if(btn$nprs > 0) {
      nLines <- (btn$nprs %/% 8 + ifelse((btn$nprs %% 8)==0, 0, 1))
      btn$timprs <- as.numeric(remove_empty_strings(strsplit(paste(btn_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]]))
      btn_lines <- btn_lines[-c(1:nLines)]
    }
  
  # Data set A18
    data_set_a18 <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
    btn$nobs <- data_set_a18[1]
    btn$nprobs <- data_set_a18[2]
    btn_lines <- btn_lines[-1]
    rm(data_set_a18)
  
  # Data set A19
    if(btn$nobs > 0) {
      btn$kobs <- NULL
      btn$iobs <- NULL
      btn$jobs <- NULL
      for(i in 1:btn$nobs) {
        data_set_a19 <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
        btn$kobs[i] <- data_set_a19[1]
        btn$iobs[i] <- data_set_a19[2]
        btn$jobs[i] <- data_set_a19[3]
        btn_lines <- btn_lines[-1]
      }
      rm(data_set_a19)
    }
  
  # Data set A20
    data_set_a20 <- remove_empty_strings(strsplit(btn_lines[1],' ')[[1]])
    btn$chkmas <- as.logical(data_set_a20[1])
    btn$nprmas <- as.numeric(data_set_a20[2])
    btn_lines <- btn_lines[-1]
    rm(data_set_a20)
  
  btn$perlen <- NULL
  btn$nstp <- NULL
  btn$tsmult <- NULL
  btn$tslngh <- list()
  btn$dt0 <- NULL
  btn$mxstrn <- NULL
  btn$ttsmult <- NULL
  btn$ttsmax <- NULL
  
  for(i in 1:btn$nper) {  
    # Data set A21
      data_set_a21 <- remove_empty_strings(strsplit(btn_lines[1],' ')[[1]])
      btn$perlen[i] <- as.numeric(data_set_a21[1])
      btn$nstp[i] <- as.numeric(data_set_a21[2])
      btn$tsmult[i] <- as.numeric(data_set_a21[3])
      if(btn$nper==1) {
        btn$sstate <- ifelse(as.character(data_set_a21[4])=='SSTATE',TRUE,FALSE)
        if(is.na(btn$sstate)) btn$sstate <- FALSE
      }
      btn_lines <- btn_lines[-1]
      rm(data_set_a21)
  
    # Data set A22
      if(btn$tsmult[i] <= 0) {
        nLines <- (btn$nstp %/% 8 + ifelse((btn$nprs %% 8)==0, 0, 1))
        btn$tslngh[[i]] <- as.numeric(remove_empty_strings(strsplit0(paste(btn_lines[1:nLines],collapse='\n'),' |\t|\n| \n|\n ')[[1]]))
        btn_lines <- btn_lines[-c(1:nLines)]
      }
    
    # Data set A23
      data_set_a23 <- as.numeric(remove_empty_strings(strsplit(btn_lines[1],' ')[[1]]))
      btn$dt0[i] <- data_set_a23[1]
      btn$mxstrn[i] <- data_set_a23[2]
      btn$ttsmult[i] <- data_set_a23[3]
      btn$ttsmax[i] <- data_set_a23[4]
      btn_lines <- btn_lines[-1]
      rm(data_set_a23)
  }
  
  class(btn) <- c('btn','mt3dms_package')
  return(btn)
}
