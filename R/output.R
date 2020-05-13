
#' Reads the mass budget from a MT3DMS listing file
#'
#' \code{rmt_read_bud} reads a mass budget from a MT3DMS listing file and returns it as a data frame
#' 
#' @param file path to the listing file; typically '*.lst'
#'
#' @return an object of class cbud which is a data.frame containing the cumulative mass budgets for all species
#' @export
#' 
rmt_read_bud <- function(file = {cat('Please select MT3DMS listing file ...\n'); file.choose()}) {
  
  # kstp, kper, tstp, tnstp, total_time, variables
  
  lst.lines <- readr::read_lines(file)
  headers <- grep(">>>>>>>FOR COMPONENT NO.", lst.lines)
  enders <- grep("DISCREPANCY (PERCENT)", lst.lines, fixed = TRUE)
  
  # if budget is printed
  if(length(headers) > 0) {
    
    # helper functions
    read_vars <- function(index, lines) rmti_remove_empty_strings(strsplit(gsub(':', ' : ', lines[index]), ' : ')[[1]][1])
    get_timing <- function(header_vector) {
      icomp <- as.numeric(strsplit(gsub('<|>', '', header_vector[1]), 'NO\\.')[[1]][2])
      # tnstp <- as.numeric(strsplit(header_vector[5], 'NO\\.')[[1]][2])
      time <- as.numeric(rmti_remove_empty_strings(strsplit(strsplit(header_vector[8], '=')[[1]][2], '\\s+')[[1]])[1])
      
      timing_line <- strsplit(header_vector[11],',')[[1]]
      tstp <- as.numeric(strsplit(timing_line[1],'TRANSPORT STEP')[[1]][2])
      kstp <- as.numeric(strsplit(timing_line[2],'TIME STEP')[[1]][2])
      kper <- as.numeric(strsplit(timing_line[3],'STRESS PERIOD')[[1]][2])
      return(list(icomp, time, tstp, kstp, kper))
    }
    get_vars <- function(var_vector) {
      breaks <- rmti_remove_empty_strings(strsplit(strsplit(gsub(':', ' : ', var_vector), ':')[[1]][2], '\\s+')[[1]])
      values <- as.numeric(breaks)
      return(values)
    }
    get_balance <- function(lines) {
      vars <- lapply(c((1:nr) + strt- 1),
                     function(i) get_vars(lines[i]))
      total_line <- strsplit(gsub(':', ' : ', lines[tot]), ':')[[1]][2]
      total_line <- rmti_remove_empty_strings(strsplit(total_line, '\\s+')[[1]])
      if(length(total_line) == 2) {
        tot_values <- list(as.numeric(total_line))
      } else {
        tot_values <- list(as.numeric(total_line[c(1,3)]))
      } 
      net_value <- as.numeric(rmti_remove_empty_strings(strsplit(gsub(':', ' : ', lines[net]), ':')[[1]][2]))
      discpr_value <- as.numeric(rmti_remove_empty_strings(strsplit(gsub(':', ' : ', lines[discrp]), ':')[[1]][2]))
      timing <- get_timing(lines)
      
      m <- do.call(cbind, as.list(unlist(c(timing, vars, tot_values, net_value, discpr_value))))
      return(m)
    } 
    clean_names <- function(var_vector) {
      name <- paste0(rmti_remove_empty_strings(strsplit(var_vector, '\\s+')[[1]]), collapse = '_')
      name <- tolower(gsub('-', '_', name))
      return(name)
    }
    get_names <- function(lines) {
      vars <- lapply(c((1:nr) + strt - 1), function(i) read_vars(index = i, lines = lines))
      
      names <- lapply(vars, clean_names)
      names <- unlist(lapply(names, function(i) paste(i, c('in', 'out'), sep = '_')))
      return(c('icomp', 'time', 'tstp', 'kstp', 'kper', names, 'total_in', 'total_out', 'difference', 'discrepancy'))
    }
    
    # call  
    # set indices based on first budget
    lines <- lst.lines[headers[1]:enders[1]]
    strt <- 16
    tot <- grep('[TOTAL]', lines, fixed = TRUE)
    end <- tot - 2
    net <- tot + 2
    discrp <- tot + 3
    
    # number of variables
    nr <- (end - strt) + 1
    names <- get_names(lines)
    
    balance <- lapply(seq_along(headers), function(i) get_balance(lst.lines[headers[i]:enders[i]]))
    balance <- as.data.frame(do.call(rbind, balance))
    colnames(balance) <- names
    
    # no budget is printed
  } else {
    # check if this is actually true
    stop("No budget was printed to listing file. You can change this in the BTN file.", call. = FALSE)
  }
  
  class(balance) <- c('cbud', class(balance))
  return(balance)
}


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
