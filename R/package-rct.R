#' Read an MT3DMS Chemical Reaction Package file
#' 
#' \code{rmt_read_rct} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} rct object.
#' 
#' @param file filename; typically '*.rct'
#' @param btn \code{RMT3DMS} btn object
#' @return object of class \code{rct}
#' @export
rmt_read_rct <- function(file = {cat('Please select rct file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())}) {
  
  rct_lines <- read_lines(file)
  rct <- NULL
  
  # Data set E1
  data_set_e1 <- as.numerical(remove_empty_strings(strsplit(rct_lines[1],' ')[[1]]))
  rct$isothm <- data_set_e1[1]
  rct$ireact <- data_set_e1[2]
  rct$irctop <- data_set_e1[3]
  rct$igetsc <- data_set_e1[4]
  rct_lines <- rct_lines[-1]  
  
  # Data set E2A
  if(rct$isothm %in% c(1,2,3,4,6)) {
    data_set_e2A <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
    rct_lines <- data_set_e2A$remaining_lines
    rct$rhob <- data_set_e2A$array
    rm(data_set_e2A)
  }
  
  # Data set E2B
  if(rct$isothm %in% c(5,6)) {
    data_set_e2B <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
    rct_lines <- data_set_e2B$remaining_lines
    rct$prsity2 <- data_set_e2B$array
    rm(data_set_e2B)
  }
  
  # Data set E2C
  rct$srconc <- list()
  if(rct$igetsc > 0) {
    for(species in 1:btn$ncomp) {
      data_set_e2C <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
      rct_lines <- data_set_e2C$remaining_lines
      rct$srconc[[species]] <- data_set_e2C$array
      rm(data_set_e2C)
    }
  }
  
  # Data set E3
  rct$sp1 <- list()
  if(rct$isothm > 0) {
    for(species in 1:btn$ncomp) {
      data_set_e3 <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
      rct_lines <- data_set_e3$remaining_lines
      rct$sp1[[species]] <- data_set_e3$array
      rm(data_set_e3)
    }
  }
  
  # Data set E4
  rct$sp2 <- list()
  if(rct$isothm > 0) {
    for(species in 1:btn$ncomp) {
      data_set_e4 <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
      rct_lines <- data_set_e4$remaining_lines
      rct$sp2[[species]] <- data_set_e4$array
      rm(data_set_e4)
    }
  }
  
  # Data set E5
  rct$rc1 <- list()
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      data_set_e5 <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
      rct_lines <- data_set_e5$remaining_lines
      rct$rc1[[species]] <- data_set_e5$array
      rm(data_set_e5)
    }
  }
  
  # Data set E6
  rct$rc2 <- list()
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      data_set_e6 <- read_mt3dms_array(rct_lines,btn$nrow,btn$ncol,btn$nlay)
      rct_lines <- data_set_e6$remaining_lines
      rct$rc2[[species]] <- data_set_e6$array
      rm(data_set_e6)
    }
  }
  
  class(rct) <- c('rct','mt3dms_package')
  return(rct)
}

#' Write an MT3DMS Chemical Reaction Package file
#' 
#' @param rct an \code{RMT3DMS} rct object
#' @param file filename to write to; typically '*.rct'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmt_write_rct <- function(rct,
                          file = {cat('Please select rct file to overwrite or provide new filename ...\n'); file.choose()},
                          btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                          iprn=-1) {
  
  # Data set E1
  cat(paste0(c(prettyNum(c(rct$isothm,rct$ireact,rct$irctop,rct$igetsc),width=10), '\n'),collapse=''), file=file)
  
  # Data set E2A
  if(rct$isothm %in% c(1,2,3,4,6)) {
    for(i in 1:btn$nlay) {
      cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(rct$rhob[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  }
  
  # Data set E2B
  if(rct$isothm %in% c(5,6)) {
    for(i in 1:btn$nlay) {
      cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
      write.table(rct$prsity2[,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
    }
  }
  
  # Data set E2C
  if(rct$igetsc > 0) {
    for(species in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$srconc[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
  
  # Data set E3
  if(rct$isothm > 0) {
    for(species in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$sp1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
  
  # Data set E4
  if(rct$isothm > 0) {
    for(species in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$sp2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
  
  # Data set E5
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$rc1[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
  
  # Data set E6
  if(rct$ireact > 0) {
    for(species in 1:btn$ncomp) {
      for(i in 1:btn$nlay) {
        cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
        write.table(rct$rc2[[species]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
      }
    }
  }
}
