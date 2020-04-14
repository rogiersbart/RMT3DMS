
#' Read an MT3DMS mass balance summary file
#' 
#' \code{rmt_read_mas} reads in an MT3DMS mass balance summary file and returns it as an \code{RMT3DMS} mas object.
#' 
#' @param file filename; typically '*.mas'
#' @return object of class mas
#' @export
rmt_read_mas <- function(file = {cat('Please select mas file ...\n'); file.choose()}) {
  names <- c('time','total_in','total_out','sources','sinks','net_mass_from_fluid_storage','total_mass_in_aquifer','discrepancy_total_in_out','discrepancy_alternative')
  mas <- readr::read_table(file, col_names = names, col_types = readr::cols(.default = readr::col_double()), skip = 2)
  class(mas) <- c('mas', class(mas))
  return(mas)
}

#' Read an MT3DMS Concentrations Observation File
#'
#' \code{rmt_read_obs} reads in an MT3DMS concentration observation file and returns it as an \code{\link{RMT3DMS}} cobs object.
#'
#' @param file filename; typically '*.obs'
#' @param btn \code{RMT3DMS} btn object
#' @param solute optional integer used to set the species index in. By default, the code tries to guess this from the filename (e.g. 'MT3D001.OBS')
#'
#' @return object of class cobs
#' @export
#'
#' @examples
rmt_read_obs <- function(file = {cat('Please select obs file ...\n'); file.choose()}, 
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                         solute = NULL) {
  nobs <- nrow(btn$obs)
  dis <- rmt_convert_btn_to_dis(btn)
  ids <- RMODFLOW::rmf_convert_ijk_to_id(i = btn$obs$i, j = btn$obs$j, k = btn$obs$k, dis = dis, type = 'r')
  
  if(is.null(solute)) {
    bs <- basename(file)
    if(grepl('MT3D([[:digit:]]{3})\\.OBS', bs, ignore.case = TRUE)) {
      solute <- as.numeric(gsub('MT3D(\\d{3})\\.OBS', '\\1', bs))
    } else {
      solute <- NULL
    }
  }
  
  # OBS files have wrapped format if nobs > 16. Difficult to use readr::read_table
  lines <- readr::read_lines(file)
  n_rm <- nobs %/% 16 + ifelse((nobs %% 16) > 0, 1, 0) + 1 # OBS file format per line is 16; line 1 has to be removed as well
  # TODO spead up line below
  values <- as.numeric(unlist(lapply(strsplit(lines[-c(1:n_rm)], '\\s'), rmti_remove_empty_strings)))
  
  df <- data.frame(matrix(values, nrow = length(values)/(2+nobs), ncol = 2 + nobs, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(df) <- c('nstp', 'tt', ids)
  
  # long format & clean
  # TODO easier with dplyr/tidyr
  df <- reshape(df, direction = 'long', varying = 3:ncol(df), v.names = 'value', times = as.character(ids))
  df$id <- as.numeric(df$time)
  df$time <- df$tt
  df$tt <- NULL
  attr(df, 'reshapeLong') <- NULL
  row.names(df) <- 1:nrow(df)
  
  # add ijk and reorder
  ijk <- RMODFLOW::rmf_convert_id_to_ijk(df$id, dis = dis, type = 'r')
  df <- cbind(df, ijk)
  df <- df[,c('nstp', 'time', 'k', 'i', 'j', 'value')]
  
  df$solute <- solute
  class(df) <- c('cobs', class(df))
  return(df)
  
}

#' Read an MT3DMS unformatted concentration file
#' 
#' \code{rmt_read_ucn} reads in an MT3DMS unformatted concentration file and returns it as an \code{\link{RMT3DMS}} ucn object.
#' 
#' @param file filename; typically '*.ucn'
#' @param btn \code{RMT3DMS} btn object
#' @param mask 3d array which can be coerced to logical indicating active (TRUE) or inactive (FALSE) cells. Concentration of inactive cells is set to NA. Defaults to the icbund array in btn.
#' @param solute optional integer used to set the species index in. By default, the code tries to guess this from the filename (e.g. 'MT3D001.UCN')
#' @param cinact concentration value(s) indicating inactive cells. These cells are set to NA. Defaults to cinact in btn.
#' @param precision either \code{'single'} (default) or \code{'double'}. Specifies the precision of the binary file.
#' @return object of class ucn and rmt_4d_array
#' @export
rmt_read_ucn <- function(file = {cat('Please select ucn file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                         mask = btn$icbund,
                         cinact = btn$cinact,
                         solute = NULL,
                         precision = 'single') {
  if(is.null(solute)) {
    bs <- basename(file)
    if(grepl('MT3D([[:digit:]]{3})\\.UCN', bs, ignore.case = TRUE)) {
      solute <- as.numeric(gsub('MT3D(\\d{3})\\.UCN', '\\1', bs))
    } else {
      solute <- NULL
    }
  }
  
  ucn <- list()
  ntrans <- kstp <- kper <- time <- vector(mode = 'numeric', length = 1)
  nbytes <- ifelse(precision == 'single', 4, 8)
  read <- TRUE
  l <- 0
  
  con <- file(file,open='rb')
  try({
    ntrans_i <- readBin(con, what = 'integer', n = 1)
    kstp_i <- readBin(con, what = 'integer', n = 1)
    kper_i <- readBin(con, what = 'integer', n = 1)
    time_i <- readBin(con, what = 'numeric', n = 1, size = nbytes)
    text <- readChar(con, nchar = 16)
    if(!length(text != 0)) read <- FALSE
    
    while(read) { # loop over time steps (unknown number in case of btn$nprs < 0 for some model settings)
      l <- l + 1
      ntrans[l] <- ntrans_i
      kstp[l] <- kstp_i
      kper[l] <- kper_i
      time[l] <- time_i

      r <- array(dim = c(btn$nrow, btn$ncol, btn$nlay))
      for(k in 1:btn$nlay) {
        ncol <- readBin(con, what = 'integer', n = 1)
        nrow <- readBin(con, what = 'integer', n = 1)
        ilay <- readBin(con, what = 'integer', n = 1) 
        
        r[,,k] <- aperm(array(readBin(con, what = 'numeric', n = btn$ncol * btn$nrow, size = nbytes), dim = c(btn$ncol, btn$nrow)), c(2,1))
        
        ntrans_i <- readBin(con, what = 'integer', n = 1)
        kstp_i <- readBin(con, what = 'integer', n = 1)
        kper_i <- readBin(con, what = 'integer', n = 1)
        time_i <- readBin(con, what = 'numeric', n = 1, size = nbytes)
        text <- readChar(con, nchar = 16)
      }
      if(!is.null(mask)) r[which(mask == 0)] <- NA
      ucn[[l]] <- r
      if(!length(text != 0)) read <- FALSE
    }
  })
  close(con)
  
  ucn <- abind::abind(ucn, along = 4)
  attr(ucn, 'dimnames') <- NULL
  ucn <- rmt_create_array(ucn, solute = solute)
  if(!is.null(cinact)) ucn[which(ucn %in% cinact)] <- NA
  attr(ucn, 'ntrans') <- ntrans
  attr(ucn, 'kstp') <- kstp
  attr(ucn, 'kper') <- kper
  attr(ucn, 'time') <- time
  
  class(ucn) <- c('ucn', class(ucn))
  return(ucn)
}
