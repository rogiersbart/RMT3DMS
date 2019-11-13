#' Read an MT3DMS basic transport package file
#' 
#' \code{read_btn} reads in an MT3DMS basic transport package file and returns it as an \code{\link{RMT3DMS}} btn object.
#' 
#' @param file filename; typically '*.btn'
#' @param ... optional arguments passed to \code{\link{rmti_parse_array}}
#' @return object of class btn
#' @export
rmt_read_btn <- function(file = {cat('Please select btn file ...\n'); file.choose()}, ...) {
  
  btn_lines <- readr::read_lines(file)
  btn <- list()
  
  # Data set 1-2
  comment(btn) <- btn_lines[c(1,2)]
  btn_lines <- btn_lines[-c(1,2)]
  
  # Data set 3
  data_set_3 <- rmti_parse_variables(btn_lines, character = TRUE, format = 'free')
  
  # Options in MT3D-USGS
  btn$modflowstylearrays <- 'MODFLOWSTYLEARRAYS' %in% toupper(data_set_3$variables)
  btn$drycell <- 'DRYCELL' %in% toupper(data_set_3$variables)
  btn$legacy99storage <- 'LEGACY99STORAGE' %in% toupper(data_set_3$variables)
  btn$ftlprint <- 'FTLPRINT' %in% toupper(data_set_3$variables)
  btn$nowetdryprint <- 'NOWETDRYPRINT' %in% toupper(data_set_3$variables)
  btn$omitdrycellbudget <- 'OMITDRYCELLBUDGET' %in% toupper(data_set_3$variables)
  btn$altwtsorb <- 'ALTWTSORB' %in% toupper(data_set_3$variables)
  
  if(sum(unlist(btn)) > 0) btn_lines <- data_set_3$remaining_lines
  
  data_set_3 <- rmti_parse_variables(btn_lines, n = 6)
  btn$nlay <- as.numeric(data_set_3$variables[1])
  btn$nrow <- as.numeric(data_set_3$variables[2])
  btn$ncol <- as.numeric(data_set_3$variables[3])
  btn$nper <- as.numeric(data_set_3$variables[4])
  btn$ncomp <- as.numeric(data_set_3$variables[5])
  btn$mcomp <- as.numeric(data_set_3$variables[6])
  btn_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # Data set 4
  data_set_4 <- rmti_parse_variables(btn_lines, character = TRUE, n = 3, width = 4)
  btn$tunit <- data_set_4$variables[1]
  btn$lunit <- data_set_4$variables[2]
  btn$munit <- data_set_4$variables[3]
  btn_lines <- data_set_4$remaining_lines
  rm(data_set_4)
  
  # Data set 5
  data_set_5 <- rmti_parse_variables(btn_lines, n = 10, width = 2)
  btn$trnop <- as.logical(data_set_5$variables[1:10])
  btn_lines <- data_set_5$remaining_lines
  rm(data_set_5)
  
  # Data set 6
  data_set_6 <- rmti_parse_variables(btn_lines, nlay = btn$nlay, width = 2)
  btn$laycon <- as.numeric(data_set_6$variables[1:btn$nlay])
  btn_lines <- data_set_6$remaining_lines
  rm(data_set_6)
  
  # Data set 7
  data_set_7 <- rmti_parse_array(btn_lines,1,btn$ncol,1, ndim = 1, file = file, ...)
  btn$delr <- data_set_7$array
  btn_lines <- data_set_7$remaining_lines
  rm(data_set_7)
  
  # Data set 8
  data_set_8 <- rmti_parse_array(btn_lines,1,btn$nrow,1, ndim = 1, file = file, ...)
  btn$delc <- data_set_8$array
  btn_lines <- data_set_8$remaining_lines
  rm(data_set_8)
  
  # Data set 9
  data_set_9 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,1, file = file, ...)
  btn_lines <- data_set_9$remaining_lines
  btn$htop <- data_set_9$array
  rm(data_set_9)
  
  # Data set 10
  data_set_10 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
  btn_lines <- data_set_10$remaining_lines
  btn$dz <- data_set_10$array
  rm(data_set_10)
  
  # Data set 11
  data_set_11 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
  btn_lines <- data_set_11$remaining_lines
  btn$prsity <- data_set_11$array
  rm(data_set_11)
  
  # Data set 12
  data_set_12 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
  btn_lines <- data_set_12$remaining_lines
  btn$icbund <- data_set_12$array
  rm(data_set_12)
  
  # Data set 13
  btn$sconc <- list()
  for(species in 1:btn$ncomp) {
    data_set_13 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
    btn_lines <- data_set_13$remaining_lines
    btn$sconc[[species]] <- rmt_create_array(data_set_13$array, solute = species)
    rm(data_set_13)
  }
  
  # Data set 14
  data_set_14 <- rmti_parse_variables(btn_lines, n = 2, width = 10)
  btn$cinact <- as.numeric(data_set_14$variables[1])
  btn$thkmin <- as.numeric(data_set_14$variables[2])
  btn_lines <- data_set_14$remaining_lines
  rm(data_set_14)
  
  # Data set 15
  data_set_15 <- rmti_parse_variables(btn_lines, n = 5, width = 10, character = TRUE)
  btn$ifmtcn <- as.numeric(data_set_15$variables[1])
  btn$ifmtnp <- as.numeric(data_set_15$variables[2])
  btn$ifmtrf <- as.numeric(data_set_15$variables[3])
  btn$ifmtdp <- as.numeric(data_set_15$variables[4])
  btn$savucn <- as.logical(data_set_15$variables[5])
  btn_lines <- data_set_15$remaining_lines   
  rm(data_set_15)
  
  # Data set 16
  data_set_16 <- rmti_parse_variables(btn_lines, n = 1, width = 10)
  btn$nprs <- as.numeric(data_set_16$variables[1])
  btn_lines <- data_set_16$remaining_lines
  rm(data_set_16)
  
  # Data set 17
  if(btn$nprs > 0) {
    data_set_17 <- rmti_parse_variables(btn_lines, nlay = btn$nprs, width = 10)
    btn$timprs <- as.numeric(data_set_17$variables[1:btn$nprs])
    btn_lines <- data_set_17$remaining_lines
    rm(data_set_17)
  }
  
  # Data set 18
  data_set_18 <- rmti_parse_variables(btn_lines, n = 2, width = 10)
  btn$nobs <- as.numeric(data_set_18$variables[1])
  btn$nprobs <- as.numeric(data_set_18$variables[2])
  btn_lines <- data_set_18$remaining_lines
  rm(data_set_18)
  
  # Data set 19
  if(btn$nobs > 0) {
    btn$kobs <- NULL
    btn$iobs <- NULL
    btn$jobs <- NULL
    for(i in 1:btn$nobs) {
      data_set_19 <- rmti_parse_variables(btn_lines, n = 3, width = 10)
      btn$kobs[i] <- as.numeric(data_set_19$variables[1])
      btn$iobs[i] <- as.numeric(data_set_19$variables[2])
      btn$jobs[i] <- as.numeric(data_set_19$variables[3])
      btn_lines <- data_set_19$remaining_lines
    }
    rm(data_set_19)
  }
  
  # Data set 20
  data_set_20 <- rmti_parse_variables(btn_lines, n = 2, width = 10, character = TRUE)
  btn$chkmas <- as.logical(data_set_20$variables[1])
  btn$nprmas <- as.numeric(data_set_20$variables[2])
  btn_lines <- data_set_20$remaining_lines
  rm(data_set_20)
  
  btn$perlen <- NULL
  btn$nstp <- NULL
  btn$tsmult <- NULL
  btn$tslngh <- list()
  btn$dt0 <- NULL
  btn$mxstrn <- NULL
  btn$ttsmult <- NULL
  btn$ttsmax <- NULL
  
  for(i in 1:btn$nper) {  
    # Data set 21
    data_set_21 <- rmti_parse_variables(btn_lines, n = 4, width = 10, character = TRUE)
    btn$perlen[i] <- as.numeric(data_set_21$variables[1])
    btn$nstp[i] <- as.numeric(data_set_21$variables[2])
    btn$tsmult[i] <- as.numeric(data_set_21$variables[3])
    variables <- rmti_parse_variables(btn_lines, format = 'free', character = TRUE)
    btn$sstate[i] <- 'SSTATE' %in% toupper(variables)
    btn_lines <- data_set_21$remaining_lines
    rm(data_set_21, variables)
    
    # Data set 22
    if(btn$tsmult[i] <= 0) {
      data_set_22 <- rmti_parse_variables(btn_lines, nlay = btn$nstp[i], width = 10)
      btn$tslngh[[i]] <- as.numeric(data_set_22$variables)
      btn_lines <- data_set_22$remaining_lines
      rm(data_set_22)
    }
    
    # Data set 23
    data_set_23 <- rmti_parse_variables(btn_lines, n = 4, width = 10)
    btn$dt0[i] <- as.numeric(data_set_23$variables[1])
    btn$mxstrn[i] <- as.numeric(data_set_23$variables[2])
    btn$ttsmult[i] <- as.numeric(data_set_23$variables[3])
    btn$ttsmax[i] <- as.numeric(data_set_23$variables[4])
    btn_lines <- data_set_23$remaining_lines
    rm(data_set_23)
  }
  if(length(btn$tslngh) == 0) btn$tslngh <- NULL
  
  class(btn) <- c('btn','rmt_package')
  return(btn)
}

#' @describeIn rmt_read_btn Deprecated function name
#' @export
read_btn <- function(...) {
  .Deprecated(new = "rmt_read_btn", old = "read_btn")
  rmt_read_btn(...)
}

#' Write an MT3DMS basic transport package file
#' 
#' @param btn an \code{\link{RMT3DMS}} btn object
#' @param file filename to write to; typically '*.btn'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmti_write_array}. 
#' @return \code{NULL}
#' @export
rmt_write_btn <- function(btn,
                      file = {cat('Please select btn file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1,
                      ...) {
  
  # Data set 1-2
  v <- packageDescription("RMT3DMS")$Version
  cat(paste('# MT3DMS Basic Transport File created by RMT3DMS, version',v,'\n'), file=file)
  cat(paste('#', comment(btn)), '\n', file=file, append=TRUE)
  
  # options
  if(!is.null(btn$options)) rmti_write_variables(btn$options, file = file, format = 'free')

  # Data set 3
  rmti_write_variables(btn$nlay, btn$nrow, btn$ncol, btn$nper, btn$ncomp, btn$mcomp, file = file)

  # Data set 4
  rmti_write_variables(btn$tunit, btn$lunit, btn$munit, file = file, width = 4)
  
  # Data set 5
  rmti_write_variables(as.character(factor(btn$trnop,levels=c(TRUE,FALSE),labels=c(' T',' F'))), file = file, width = 2)
  
  # Data set 6
  nLines <- (btn$nlay %/% 40 + ifelse((btn$nlay %% 40) == 0, 0, 1))
  for(i in 1:nLines) {
    rmti_write_variables(btn$laycon[((i-1)*40+1) : ifelse((i*40) > btn$nlay, btn$nlay, (i*40))], file = file, width = 2)
  }
  
  # Data set 7
  rmti_write_array(btn$delr, file = file, iprn = iprn, ...)
  
  # Data set 8
  rmti_write_array(btn$delc, file = file, iprn = iprn, ...)
  
  # Data set 9
  rmti_write_array(btn$htop, file = file, iprn = iprn, ...)
  
  # Data set 10
  rmti_write_array(btn$dz, file = file, iprn = iprn, ...)
  
  # Data set 11
  rmti_write_array(btn$prsity, file = file, iprn = iprn, ...)
  
  # Data set 12
  rmti_write_array(btn$icbund, file = file, iprn = iprn, ...)
  
  # Data set 13
  for(species in 1:btn$ncomp) {
    btn$sconc[[species]][which(is.na(btn$sconc[[species]]))] <- btn$cinact
    rmti_write_array(btn$sconc[[species]], file = file, iprn = iprn, ...)
  }
  
  # Data set 14
  rmti_write_variables(btn$cinact, btn$thkmin, file = file)
  
  # Data set 15
  rmti_write_variables(btn$ifmtcn, btn$ifmtnp, btn$ifmtrf, btn$ifmtdp, as.character(factor(btn$savucn,levels=c(TRUE,FALSE),labels=c(' T',' F'))), file = file)
  
  # Data set 16
  rmti_write_variables(btn$nprs, file = file)
  
  # Data set 17
  if(btn$nprs > 0) {
    nLines <- (btn$nprs %/% 8 + ifelse((btn$nprs %% 8) == 0, 0, 1))
    for(i in 1:nLines) {
      rmti_write_variables(btn$timprs[((i-1)*8+1) : ifelse((i*8) > btn$nprs, btn$nprs, (i*8))], file = file)
    }
  }
  
  # Data set 18
  rmti_write_variables(btn$nobs, btn$nprobs, file = file)
  
  # Data set 19
  if(btn$nobs > 0) {
    for(i in 1:btn$nobs) {
      rmti_write_variables(btn$kobs[i], btn$iobs[i], btn$jobs[i], file = file)
    }
  }
  
  # Data set 20
  rmti_write_variables(as.character(factor(btn$chkmas,levels = c(TRUE,FALSE),labels=c('T','F'))), btn$nprmas, file = file)
  
  for(i in 1:btn$nper) {  
    # Data set 21
    rmti_write_variables(btn$perlen[i], btn$nstp[i], btn$tsmult[i], ifelse(btn$sstate[i], 'SSTATE', ''), file = file)
    
    # Data set 22
    if(btn$tsmult[i] <= 0) {
      nLines <- (btn$nstp %/% 8 + ifelse((btn$nstp %% 8) == 0, 0, 1))
      for(j in 1:nLines) {
        rmti_write_variables(btn$tslngh[((j-1)*8+1) : ifelse((j*8) > btn$nstp, btn$nstp, (j*8))], file = file)
      }
    }
    
    # Data set 23
    rmti_write_variables(btn$dt0[i], btn$mxstrn[i], btn$ttsmult[i], btn$ttsmax[i], file = file)
  }
}

#' @describeIn rmt_write_btn Deprecated function name
#' @export
write_btn <- function(...) {
  .Deprecated(new = "rmt_write_btn", old = "write_btn")
  rmt_write_btn(...)
}
