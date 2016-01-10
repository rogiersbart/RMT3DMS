#' Read an MT3DMS sink & source mixing package file
#' 
#' \code{read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \code{\link{RMT3DMS}} ssm object.
#' 
#' @param file filename; typically '*.ssm'
#' @param btn basic transport package file object
#' @return object of class ssm
#' @importFrom readr read_lines
#' @export
read_ssm <- function(file = {cat('Please select ssm file ...\n'); file.choose()},
                     btn = read_btn()) {
  
  ssm_lines <- read_lines(file)
  ssm <- NULL
  
  # Data set D1
    data_set_d1 <- as.logical(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
    ssm$fwel <- data_set_d1[1]
    ssm$fdrn <- data_set_d1[2]
    ssm$frch <- data_set_d1[3]
    ssm$fevt <- data_set_d1[4]
    ssm$friv <- data_set_d1[5]
    ssm$fghb <- data_set_d1[6]
    ssm$fnew <- data_set_d1[7:10]
    ssm_lines <- ssm_lines[-1]  
  
  # Data set D2
    ssm$mxss <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
    ssm_lines <- ssm_lines[-1]  
  
  ssm$incrch <- NULL
  ssm$crch <- list()
  ssm$incevt <- NULL
  ssm$cevt <- list()
  ssm$nss <- NULL
  ssm$kss <- list()
  ssm$iss <- list()
  ssm$jss <- list()
  ssm$css <- list()
  ssm$itype <- list()
  ssm$cssms <- list()
  
  for(stress_period in 1:btn$nper) {
    # Data set D3
      if(ssm$frch) {
        ssm$incrch[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
        ssm_lines <- ssm_lines[-1] 
        
        # Data set D4
          if(ssm$frch & (ssm$incrch[stress_period] >= 0)) {
            data_set_d4 <- read_mt3dms_array(ssm_lines,btn$nrow,btn$ncol,btn$ncomp)
            ssm_lines <- data_set_d4$remaining_lines
            ssm$crch[[stress_period]] <- data_set_d4$array
            rm(data_set_d4)
          }
      }
    
    # Data set D5
      if(ssm$fevt) {
        ssm$incevt[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
        ssm_lines <- ssm_lines[-1] 
    
        # Data set D6
          if(ssm$fevt & (ssm$incevt[stress_period] >= 0)) {
            data_set_d6 <- read_mt3dms_array(ssm_lines,btn$nrow,btn$ncol,btn$ncomp)
            ssm_lines <- data_set_d6$remaining_lines
            ssm$cevt[[stress_period]] <- data_set_d6$array
            rm(data_set_d6)
          }
      }
    
    # Data set D7
      ssm$nss[stress_period] <- as.numeric(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
      ssm_lines <- ssm_lines[-1] 
    
    ssm$kss[[stress_period]] <- rep(NA,ssm$nss[stress_period])
    ssm$iss[[stress_period]] <- ssm$kss
    ssm$jss[[stress_period]] <- ssm$kss
    ssm$css[[stress_period]] <- ssm$kss
    ssm$itype[[stress_period]] <- ssm$kss
    ssm$cssms[[stress_period]] <- list()
    
    # Data set D8
      if(ssm$nss > 0) {
        for(i in 1:ssm$nss[stress_period]) {
          data_set_d8 <- as.logical(remove_empty_strings(strsplit(ssm_lines[1],' ')[[1]]))
          ssm$kss[[stress_period]][i] <- data_set_d8[1]
          ssm$iss[[stress_period]][i] <- data_set_d8[2]
          ssm$jss[[stress_period]][i] <- data_set_d8[3]
          ssm$css[[stress_period]][i] <- data_set_d8[4]
          ssm$itype[[stress_period]][i] <- data_set_d8[5]
          ssm$cssms[[stress_period]][[i]] <- data_set_d8[6:(6+btn$ncomp-1)]
          ssm_lines <- ssm_lines[-1] 
        }
      }
  }
  
  class(ssm) <- c('ssm','mt3dms_package')
  return(ssm)
}
