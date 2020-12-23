
#' Reads the mass budget from a MT3DMS listing file
#'
#' \code{rmt_read_bud} reads a mass budget from a MT3DMS listing file and returns it as a data frame
#' 
#' @param file path to the listing file; typically '*.m3d'
#'
#' @return an object of class cbud which is a data.frame containing the cumulative mass budgets for all species
#' @export
#' 
rmt_read_bud <- function(file = {cat('Please select MT3DMS listing file ...\n'); file.choose()}) {
  
  # kstp, kper, tstp, tnstp, total_time, variables
  
  lst.lines <- readr::read_lines(file)
  headers <- grep(">>>>>>>FOR COMPONENT NO.", lst.lines)
  enders <- grep("[TOTAL]", lst.lines, fixed = TRUE)
  
  # if budget is printed
  if(length(headers) > 0) {
    
    # helper functions
    read_vars <- function(index, lines) rmti_remove_empty_strings(strsplit(gsub(':', ' : ', lines[index]), ' : ')[[1]][1])
    get_timing <- function(header_vector) {
      icomp <- as.numeric(strsplit(gsub('<|>', '', header_vector[1]), 'NO\\.')[[1]][2])
      # tnstp <- as.numeric(strsplit(header_vector[5], 'NO\\.')[[1]][2])
      time <- as.numeric(rmti_remove_empty_strings(strsplit(paste0(strsplit(strsplit(header_vector[8], '=')[[1]][2], '')[[1]][1:15], collapse = ''), '\\s+')[[1]])[1])
      
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
    enders <- enders + 3
    lines <- lst.lines[headers[1]:enders[1]]
    strt_budg <- grep('LATIVE MASS BUDGETS AT END', lines)
    strt <- strt_budg + 5
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
  
  balance <- tibble::tibble(balance)
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
  df <- tibble::tibble(df)
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


#' Read a MT3DMS observed concentration file
#'
#' \code{rmt_read_ocn} reads in an MT3DMS observed concentration file and returns it as an \code{\link{RMT3DMS}} ocn object.
#'
#' @param file filename; typically '*.ocn'
#' @details Writing and type of OCN output is set in the TOB file.
#' @return object of class ocn and data.frame
#' @export
#' @seealso \code{\link{rmt_read_mfx}}, \code{\link{rmt_read_pst}} and \code{\link{rmt_create_tob}}
rmt_read_ocn <- function(file = {cat('Please select ocn file ...\n'); file.choose()}) {
  
  ocn_lines <- readr::read_lines(file)
  
  sp_id <- grep('STRESS PERIOD', ocn_lines)
  wellid <- grep('WELLID', ocn_lines)
  strt_id <- wellid + 2
  end_id <- c(sp_id[-1] - 4, length(ocn_lines) - 1)
  
  # check if last line is not empty
  if(length(rmti_remove_empty_strings(ocn_lines[length(ocn_lines)])) != 0) end_id[length(end_id)] <- end_id[length(end_id)] + 1

  # check for written statistics
  stats <- grep('PROBABILITY OF UN-CORRELATION =', ocn_lines[end_id])
  if(length(stats) > 0) end_id[stats] <- end_id[stats] - 8
  
  res <- any(grepl('WEIGHT', ocn_lines))
  logd <- any(grepl('LogCAL', ocn_lines))
  nms <- c('stress_period', 'time_step', 'transport_step', 'total_elapsed_time', 'wellid', 'x_grid', 'y_grid', 'layer', 'row', 'column',
           'species', 'calculated')
  if(res) nms <- c(nms, 'observed', 'weight', ifelse(logd, 'log_residual', 'residual'))
  cpr <- setNames(data.frame(matrix(ncol = length(nms), nrow = 0)), nms)
  
  # helper function
  read_data <- function(sp, start, end) {
    stress <- as.numeric(strsplit(ocn_lines[sp], ':')[[1]][2])
    fts <- as.numeric(strsplit(ocn_lines[sp + 1], ':')[[1]][2])
    tts <- as.numeric(strsplit(ocn_lines[sp + 2], ':')[[1]][2])
    time <- as.numeric(strsplit(ocn_lines[sp + 3], ':')[[1]][2])
    
    empty <- grepl('No obs wells active at', ocn_lines[start])
    if(length(rmti_remove_empty_strings(strsplit(ocn_lines[start], ' ')[[1]])) == 0) empty <- TRUE
    if(empty) return(NULL)
    
    df <- cpr
    for(i in 1:(end-start+1)) {
      ln <- start + i - 1
      no_obs <- grepl('no observed conc given', ocn_lines[ln])
      vl <- rmti_remove_empty_strings(strsplit(ocn_lines[ln], ' ')[[1]])
      name <- vl[1]
      if(res) {
        if(no_obs) {
          nums <- c(vl[2:8], rep(NA, 3))
        } else {
          nums <- vl[2:11]
        }
      } else {
        nums <- vl[2:8]
      }
      
      df[i, nms[1]] <- stress
      df[i, nms[2]] <- fts
      df[i, nms[3]] <- tts
      df[i, nms[4]] <- time
      df[i, nms[5]] <- name
      df[i, nms[6:length(nms)]] <- as.numeric(nums)
    }

    return(df)
  }
  
  df.lst <- lapply(1:length(sp_id), function(i) read_data(sp_id[i], strt_id[i], end_id[i]))
  cpr.df <- do.call(rbind, df.lst)
  if(is.null(cpr.df)) cpr.df <- cpr
  cpr.df <- tibble::tibble(cpr.df)
  class(cpr.df) <- c('ocn', class(cpr.df))
  return(cpr.df)
}

#' Read a MT3DMS mass flux file
#'
#' \code{rmt_read_mfx} reads in an MT3DMS mass flux file and returns it as an \code{\link{RMT3DMS}} mfx object.
#'
#' @param file filename; typically '*.mfx'
#' @details Writing and type of MFX output is set in the TOB file.
#' @return object of class mfx and data.frame
#' @export
#' @seealso \code{\link{rmt_read_mfx}}, \code{\link{rmt_read_pst}} and \code{\link{rmt_create_tob}}
rmt_read_mfx <- function(file = {cat('Please select mfx file ...\n'); file.choose()}) {
  
  mfx_lines <- readr::read_lines(file)
  
  sp_id <- grep('STRESS PERIOD', mfx_lines)
  groupid <- grep('NO.', mfx_lines)
  strt_id <- groupid + 2
  end_id <- c(sp_id[-1] - 4, length(mfx_lines) - 1)
  
  # check if last line is not empty
  if(length(rmti_remove_empty_strings(mfx_lines[length(mfx_lines)])) != 0) end_id[length(end_id)] <- end_id[length(end_id)] + 1
  
  # check for written statistics
  stats <- grep('PROBABILITY OF UN-CORRELATION =', mfx_lines[end_id])
  if(length(stats) > 0) end_id[stats] <- end_id[stats] - 8
  
  res <- any(grepl('WEIGHT', mfx_lines))
  logd <- any(grepl('LogCAL', mfx_lines)) # not possible
  nms <- c('stress_period', 'time_step', 'transport_step', 'total_elapsed_time', 'group.no', 'name', 'time', 'species', 'calculated')
  if(res) nms <- c(nms, 'observed', 'weight', ifelse(logd, 'log_residual', 'residual'))
  cpr <- setNames(data.frame(matrix(ncol = length(nms), nrow = 0)), nms)
  
  # helper function
  read_data <- function(sp, start, end) {
    stress <- as.numeric(strsplit(mfx_lines[sp], ':')[[1]][2])
    fts <- as.numeric(strsplit(mfx_lines[sp + 1], ':')[[1]][2])
    tts <- as.numeric(strsplit(mfx_lines[sp + 2], ':')[[1]][2])
    time <- as.numeric(strsplit(mfx_lines[sp + 3], ':')[[1]][2])
    
    empty <- grepl('No flux object active at', mfx_lines[start])
    if(length(rmti_remove_empty_strings(strsplit(mfx_lines[start], ' ')[[1]])) == 0) empty <- TRUE
    if(empty) return(NULL)
    
    df <- cpr
    for(i in 1:(end-start+1)) {
      ln <- start + i - 1
      no_obs <- grepl('no observed flux given', mfx_lines[ln])
      vl <- rmti_remove_empty_strings(strsplit(mfx_lines[ln], ' ')[[1]])
      group <- as.numeric(vl[1])
      name <- vl[2]
      if(res) {
        if(no_obs) {
          nums <- c(vl[3:5], rep(NA, 3))
        } else {
          nums <- vl[3:8]
        }
      } else {
        nums <- vl[3:5]
      }
      
      df[i, nms[1]] <- stress
      df[i, nms[2]] <- fts
      df[i, nms[3]] <- tts
      df[i, nms[4]] <- time
      df[i, nms[5]] <- group
      df[i, nms[6]] <- name
      df[i, nms[7:length(nms)]] <- as.numeric(nums)
    }
    
    return(df)
  }
  
  df.lst <- lapply(1:length(sp_id), function(i) read_data(sp_id[i], strt_id[i], end_id[i]))
  cpr.df <- do.call(rbind, df.lst)
  if(is.null(cpr.df)) cpr.df <- cpr
  cpr.df <- tibble::tibble(cpr.df)
  class(cpr.df) <- c('mfx', class(cpr.df))
  return(cpr.df)
}

# rmt_read_pst <- function(file, precision = 'single') {
#   
#   nbytes <- ifelse(precision == 'single', 4, 8)
#   read <- TRUE
#   l <- 0
#   
#   con <- file(file,open='rb')
#   (text <- readChar(con, nchar = 4))
#   (text <- readChar(con, nchar = 15))
#   (text <- readChar(con, nchar = 12))
#   
#   close(con)
#   
#   try({
#     
#     (text <- readChar(con, nchar = 6))
#     
#     
#     ntrans_i <- readBin(con, what = 'integer', n = 1)
#     kstp_i <- readBin(con, what = 'integer', n = 1)
#     kper_i <- readBin(con, what = 'integer', n = 1)
#     time_i <- readBin(con, what = 'numeric', n = 1, size = nbytes)
#     if(!length(text != 0)) read <- FALSE
#     
#   })
#   close(con)
#   
#   
#   
# }