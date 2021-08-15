

#' Function to create a rmt_list object from RMODFLOW discete boundary condition object objects
#'
#' @param obj RMODFLOW discete boundary condition object. Allowed objects are chd, ghb, riv, drn, wel
#' @param conc vector, data.frame or matrix with concentration values. 
#' @param itype integer column (or single value) to indicate flux type
#' @param kper integer value(s) indicating during which stress-periods this rmf_list is active
#'
#' @return a \code{rmt_list} object
#' @keywords internal
rmti_create_bc_list <- function(obj, conc, itype, kper) {
  
  # kper can only be different if flow has only 1 stress-period (should be nr 1) which should be steady-state (not known in this code)
  kper_flow <- unique(obj$kper$kper)
  if(!identical(sort(kper_flow), sort(unique(kper)))) {
    if(length(kper_flow) > 1 || kper_flow != 1) {
      stop('kper of rmt_list differs from kper of flow object', call. = FALSE)
    } else {
      kper_select <- 1
    }
  } else {
    kper_select <- kper
  }
  
  names_act <- colnames(obj$kper)[which(obj$kper[kper_select,which(!is.na(obj$kper[kper_select,]))] != FALSE)[-1]]
  if(length(names_act) == 0) stop('No active obj features for kper ', kper_select, call. = FALSE)
  
  df <- as.data.frame(obj$data)
  df <- df[df$name %in% names_act, ]
  
  # concentrations
  conc <- structure(as.data.frame(matrix(conc, ncol = ncol(conc))), names = paste0('css', 1:ncol(conc)))
  df <- cbind(df, itype, conc)
  df <- rmt_create_list(df, kper = kper)
  return(df)
}

#' Conditional return
#' 
#' \code{rmti_ifelse0} returns \code{yes} if \code{test} is \code{TRUE}. If \code{test} is \code{FALSE}, it returns \code{no}.
#' @param test an object which can be coerced to logical mode.
#' @param yes return value for \code{test==TRUE}
#' @param no return value for \code{test==FALSE}
#' @keywords internal
rmti_ifelse0 <- function(test, yes, no) {
  if(test)   {
    return(yes)
  } else {
    return(no)
  }
}

#' List supported MT3DMS/MT3D-USGS packages
#'
#' @param type character denoting type of packages to list; possible values are \code{'usgs' (default), 'mt3dms', 'output'}
#'
#' @return data.frame with ftype and rmt columns denoting the MT3DMS and \code{RMT3DMS} abbreviations for the requested packages
#' @keywords internal
#' @details 'usgs' holds all packages; 'mt3dms' is a subset, 'output' lists all supported output types
#' @note this function should be updated every time a new MT3DMS package is supported in \code{RMT3DMS}
rmti_list_packages <- function(type = 'usgs') {
  
  # update rmfd_supported_packages in /data-raw/ when a new package is supported
  # NAM file is not in here but is supported
  df <- rmtd_supported_packages
  
  # Below is an exhaustive overview of all packages in MT3D-USGS & MT3DMS (latter is a subset of the former)
  # MT3D-USGS
  usgs <- c('btn', 'ft6', 'adv', 'dsp', 'ssm', 'rct', 'gcg', 'tob', 'hss', 'cts', 'tso', 'uzt', 'lkt', 'sft')
  
  # MT3DMS
  mt3dms <- c('btn', 'adv', 'dsp', 'ssm', 'rct', 'gcg', 'tob', 'hss')
  
  # subset or output
  if(type == 'output') {
    df <- rmtd_supported_output
  } else {
    df <- subset(df, rmt %in% get(type))
  }

  return(df)
}

#' Read comments
#' Internal function used in the read_* functions to read comments
#' @param id optional integers specifying which lines are comments. If NULL (default), lines starting with "#" indicate commented lines
#' @details removes empty comments and prevents copying of RMT3DMS header comment 
#' @keywords internal
rmti_parse_comments <- function(remaining_lines, id = NULL) {
  v <- paste("RMT3DMS, version",  packageDescription("RMT3DMS")$Version)
  comments <- NULL
  if(is.null(id)) {
    comment_tag <- substr(remaining_lines, 1, 1)
    comment_id <- which(comment_tag == "#")
  } else {
    comment_id <- id
  }
  
  if(length(comment_id) > 0) {
    comments <- gsub('#', '', remaining_lines[comment_id])
    
    # remove empty comments
    empty <- which(nchar(trimws(comments)) == 0)
    if(length(empty) > 0) comments <- comments[-empty]
    
    # remove RMT3DMS header
    header <- grep(v, comments)
    if(length(header) > 0) comments <- comments[-header]
    
    remaining_lines <- remaining_lines[-comment_id]
  }
  
  return(list(comments = comments, remaining_lines = remaining_lines))
}

#' Read the package header from a flow-transport link file
#'
#' @param file pathname to the flow-transport link file, typically '*.ftl'
#' @param binary is the FTL file binary?
#'
#' @return logical vector of length 3, indicating if the rch, evt or uzf packages are active in the flow simulation
#' @details This function is used in \code{\link{rmt_read_ssm}} as a replacement for reading data set 1. It is used to determine if
#'          rch, evt and/or uzf concentration arrays have to be read. All other active flow packages are read from the point source/sink data sets.
#'          \code{rmti_parse_ftl_header} can only be used with flow-transport link files using MT3DMS or MT3D-USGS headers.
#'          The UZF package can only be used with MT3D-USGS.
#' @keywords internal
rmti_parse_ftl_header <- function(file, binary = NULL) {
  
  lines <- suppressWarnings(readr::read_lines(file, n_max = 40, lazy = FALSE)) # warning from readr with parsing issues if file is binary
  lg <- structure(rep(FALSE, 3), names = c('frch', 'fevt', 'fuzf'))
  
  # binary <- FALSE
  # v <- suppressWarnings(strsplit(lines[1], '')[[1]])
  # if(length(v) == 1 && is.na(v)) binary <- TRUE
  
  if(!is.na(lines[2])) {
    if(is.null(binary)) {
      # TODO this is a weak check for binary
      binary <- !validUTF8(lines[2])
      if(lines[2] == '') binary <- TRUE
    }

    if(binary) { # binary
      con <- file(file, open = 'rb')
      try({
        v <- readChar(con, nchars = 11)
        rec <- readBin(con, what = 'integer', n = 9)
        if(rec[3] > 0) lg['frch'] <- TRUE
        if(rec[4] > 0) lg['fevt'] <- TRUE
        
        # s <- grepl('MT3D', v, ignore.case = TRUE) # MT3DMS header
        usgs <- grepl('MTGS', v, ignore.case = TRUE) # MT3D-USGS
        if(usgs) {
          npk <- readBin(con, what = 'integer', n = 1)
          if(npk > 0) {
            rec2 <- vector(mode = 'character', length = npk)
            for(i in 1:npk) rec2[i] <- toupper(trimws(readChar(con, nchars = 20)))
            if(any(rec2 == 'UZF')) lg['fuzf'] <- TRUE
          } 
        }
        
        # if(s) {
        #   version <- sub('MT3D', '', v, ignore.case = TRUE) 
        #   vn <- as.numeric(strsplit(version, '\\.')[[1]][1])
        #   # standard header (v < 4, not supported by MODFLOW-2005) or extended header (v >= 4)
        #   # not necessary to read
        #   # ext_header <- vn >= 4
        #   # if(ext_header) rec2 <- readBin(con, what = 'integer', n = 12)
        #   
        # } else if(usgs) {
        #   npk <- readBin(con, what = 'integer', n = 1)
        #   rec2 <- vector(mode = 'character', length = npk)
        #   for(i in 1:npk) rec2[i] <- readChar(con, nchars = 12)
        #   
        # } else {
        #   stop('Can only read flow-transport link with MT3DMS or MT3D-USGS headers', call. = FALSE)
        # }
      })
      close(con)
      
    } else { # ASCII
      rec <- rmti_parse_variables(lines, n = 10, format = 'free', character = TRUE)
      v <- trimws(rec$variables[1])
      if(as.numeric(rec$variables[4]) > 0) lg['frch'] <- TRUE
      if(as.numeric(rec$variables[5]) > 0) lg['fevt'] <- TRUE
      
      usgs <- grepl('MTGS', v, ignore.case = TRUE) # MT3D-USGS
      if(usgs) {
        remaining_lines <- rec$remaining_lines
        # if(length(rec$variables) < 9) remaining_lines <- remaining_lines[-1]
        ds <- rmti_parse_variables(remaining_lines, n = 1, format = 'free')
        npk <- as.numeric(ds$variables)
        remaining_lines <- ds$remaining_lines
        
        if(npk > 0) {
          rec2 <- trimws(gsub('\'', '', remaining_lines[1:npk]))
          if(any(toupper(rec2) == 'UZF')) lg['fuzf'] <- TRUE
        }
      }
    }
  }
  
  return(lg)
}

#' Get an array specified by a control record from the text lines analyzed in a \code{RMT3DMS} \code{rmt_read_*} function
#' @param remaining_lines lines to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read
#' @param ndim dimensions of the array to read; either 1, 2 or 3. Denotes the if the returned array should be 1D, 2D or 3D.
#' @param skip_header optional; should the control record be skipped
#' @param nam a \code{RMT3DMS} nam object. Required when reading external arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param file pathname to the input file which is currently being read. Required when reading fixed-format or MODFLOW-style OPEN/CLOSE arrays
#' @param integer logical; does the binary array hold integer values. Might not work optimally.
#' @param ... ignored
#' @return A list containing the array and the remaining text of the MT3DMS input file
#' @keywords internal
rmti_parse_array <- function(remaining_lines, nrow, ncol, nlay, ndim,
                             skip_header = FALSE, nam = NULL, precision = "single", file = NULL, integer = FALSE, ...) {
  
  
  # Initialize array object
  array <- array(dim=c(nrow,ncol,nlay))
  
  # Read array according to format type if there is anything to be read
  if(prod(dim(array))!=0)
  {
    for(k in 1:nlay) 
    { 
      header <- rmti_parse_variables(remaining_lines[1], n = 2, format = 'fixed')
      
      # MODFLOW-style free format control header
      if(header$variables[1] %in% c('CONSTANT', 'INTERNAL', 'EXTERNAL', 'OPEN/CLOSE') || skip_header) {
        rmf_data_set <- RMODFLOW:::rmfi_parse_array(remaining_lines, nrow = nrow, ncol = ncol, nlay = 1, ndim = ndim, 
                                                skip_header = skip_header, nam = nam, precision = precision, file = file, integer = integer, ...)
        array[,,k] <- rmt_convert_rmf_to_rmt(rmf_data_set$array)
        remaining_lines <- rmf_data_set$remaining_lines
        rm(rmf_data_set)
        
      } else {
        # MT3DMS fixed format control header
        
        fortranfmt <-  FALSE
        
        iread <-  as.numeric(header$variables[1])
        cnst <- as.numeric(header$variables[2])
        fmtin <-  trimws(paste0(strsplit(remaining_lines[1], split = '')[[1]][21:40], collapse = ''))
        
        if(iread == 0) {  # CONSTANT
          array[,,k] <- cnst
          nLines <- 1
          
        } else if(iread == 100) {  # INTERNAL-ARRAY
          lengths <- RMODFLOW:::rmfi_fortran_format(fmtin)
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
          nPerLine <- length(lengths)
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
          array[,,k] <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          array[,,k] <- array[,,k]*cnst
          
        } else if(iread == 101) {  # INTERNAL-BLOCK
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          ds <- rmti_parse_variables(remaining_lines, n = 1, format = 'free')
          nblock <- as.numeric(ds$variables[1])
          remaining_lines <- ds$remaining_lines
          nLines <- 0
          
          for(block in 1:nblock) {
            ds2 <- rmti_parse_variables(remaining_lines, n = 5, format = 'free')
            block_values <- as.numeric(ds2$variables[1:5])
            array[block_values[1]:block_values[2], block_values[3]:block_values[4], k] <- block_values[5]
            remaining_lines <- ds2$remaining_lines
          }
          array[,,k] <- array[,,k]*cnst
          
        } else if(iread == 102) {  # INTERNAL-ZONE
          lengths <- RMODFLOW:::rmfi_fortran_format(fmtin)
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          
          ds <- rmti_parse_variables(remaining_lines, n = 1, format = 'free')
          nzone <- as.numeric(ds$variables[1])
          remaining_lines <- ds$remaining_lines
          ds2 <- rmti_parse_variables(remaining_lines, n = nzone, format = 'free')
          zv <- as.numeric(ds2$variables[1:nzone])
          remaining_lines <- ds2$remaining_lines
          zone <- array(dim = c(nrow, ncol))
          
          remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
          nPerLine <- length(lengths)
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
          # zone <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          zone <- matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          
          for(nz in 1:nzone) {
            zone[which(zone == nz)] <- zv[nz] 
          }
          array[,,k] <- zone*cnst
          
        } else if(iread == 103) {  # INTERNAL-LIST
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          
          n_final <- nrow*ncol
          n <- 0
          nLines <- 0
          values <- vector(mode = 'numeric')
          end <- FALSE
          
          while(n < n_final && !end) {
            nLines <- nLines + 1
            values_ch <- rmti_remove_empty_strings(strsplit(paste(remaining_lines[nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])
            
            # terminate if / is encountered
            slash <- grepl('/', values_ch)
            if(any(slash)) {
              last_value <- which(slash)[1]
              values_ch[last_value] <- sub('/.*', '', values_ch[last_value])
              values_ch <- values_ch[1:last_value]
              end <- TRUE
            }
            
            values_ch <- strsplit(values_ch, '\\*')
            values_ch <- lapply(values_ch, function(i) rmti_ifelse0(length(i) > 1, rep(as.numeric(i[[2]]), as.numeric(i[[1]])), as.numeric(i)))
            values <- c(values, unlist(values_ch))
            
            n <- length(values)
          }
          
          array[,,k] <- matrix(as.numeric(values[1:n_final])*cnst, nrow = nrow, ncol = ncol, byrow = TRUE)
          
          
        } else {  # EXTERNAL
          if(cnst == 0) cnst <-  1.0
          
          if(is.null(nam)) stop('Please supply a MT3DMS nam object when reading EXTERNAL arrays', call. = FALSE)
          
          if(iread %in% nam$nunit) {
            fname <- nam$fname[which(nam$nunit == iread)]
          } else if(any(nam$nunit == 0) && iread %in% rmtd_internal_nunit$nunit) { # nunit = 0
            ext_ftype <- rmtd_internal_nunit$ftype[which(rmtd_internal_nunit$nunit == iread)]
            fname <- nam$fname[which(nam$ftype == ext_ftype)]
          } else {
            stop('nunit for EXTERNAL array not found in NAM file', call. = FALSE)
          }
          direct <-  attr(nam, 'dir')
          absfile <-  file.path(direct, fname)
          ext_file <- TRUE
          
          # ASCII
          if(iread > 0) {
            if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
              lengths <- RMODFLOW:::rmfi_fortran_format(fmtin)
              fortranfmt <-  TRUE
            }
            if(iread == nam$nunit[which(basename(nam$fname) == basename(file))] || normalizePath(absfile) == normalizePath(file)) { # read from current file
              ext_file <- FALSE
              
              remaining_lines <- remaining_lines[-1] 
              if(fortranfmt) {
                remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
                nPerLine <- length(lengths)
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
                array[,,k] <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
                
              } else {
                nPerLine <- length(as.numeric(rmti_remove_empty_strings(strsplit(remaining_lines[1],' |\t|,')[[1]])))
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                array[,,k] <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
              }
              
            } else { # read from external file
              external_lines <-  readr::read_lines(absfile)
              # remove lines of previous arrays
              if(!is.null(attr(nam, as.character(iread)))) external_lines <- external_lines[-c(1:attr(nam, as.character(iread)))]
              
              if(fortranfmt) {
                external_lines[1] <- paste(substring(external_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
                nPerLine <- length(lengths)
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                if(nLines > 1) external_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(external_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
                array[,,k] <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
                
              } else {
                nPerLine <- length(as.numeric(rmti_remove_empty_strings(strsplit(external_lines[1],' |\t|,')[[1]])))
                nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
                array[,,k] <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(external_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
              }
            }
            
          } else if(iread < 0) { # read binary from external file
            con <- file(asbfile,open='rb')
            real_number_bytes <- ifelse(precision == 'single', 4, 8)
            type <- ifelse(integer, 'integer', 'numeric')
            size <- ifelse(type == 'integer', NA_integer_, real_number_bytes)
            if(type=='integer') warning('Reading integer binary EXTERNAL array might not work optimally')
            
            try({          
              if(!is.null(attr(nam, as.character(iread)))) {
                for(jj in 1:attr(nam, as.character(iread))) {
                  invisible(readBin(con, what = 'integer', n = 2))
                  invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                  invisible(readChar(con,nchars=16))
                  nncol <- readBin(con, what = 'integer', n = 1)
                  nnrow <- readBin(con, what = 'integer', n = 1)
                  invisible(readBin(con, what = 'integer', n = 1))
                  invisible(readBin(con,what='numeric',n = nncol * nnrow, size = real_number_bytes))
                }
              }
              # integer binary arrays should not have headers in MODFLOW (2005, v1.12 - see U2DINT subroutine, line 682)
              if(!integer) {
                invisible(readBin(con, what = 'integer', n = 2))
                invisible(readBin(con,what='numeric',n = 2, size = real_number_bytes))
                invisible(readChar(con,nchars=16))
                invisible(readBin(con, what = 'integer', n = 3))
              }
              
              array[,,k] <- cnst*aperm(array(readBin(con,what=type,n = ncol * nrow, size = size),dim=c(ncol, nrow)), c(2, 1))
              nLines <-  1})
            
            close(con)
          }
          
          if(ext_file) {
            if(is.null(attr(nam, as.character(iread)))) {
              attr(nam, as.character(iread)) <- nLines
            } else {
              attr(nam, as.character(iread)) <- attr(nam, as.character(iread)) + nLines
            }
            nLines <- 1
          }
        }
        
        if(nLines > 0) remaining_lines <- remaining_lines[-c(1:nLines)]
        
      }
    }
  }
  
  # Set class of object (2darray; 3darray)
  if(ndim == 1) {
    array <- c(array(array,dim=nrow*ncol*nlay))
  } else if(ndim == 2) {
    array <- rmt_create_array(array[,,1], dim = c(nrow, ncol))
  } else if(ndim == 3) {
    array <- rmt_create_array(array, dim = c(nrow, ncol, nlay))
  } else {
    stop('ndim should be 1, 2 or 3')
  }
  
  # Return output of reading function 
  data_set <- list(array = array, remaining_lines = remaining_lines)
  return(data_set)
}

#' Read MT3DMS single-line variables
#' @param n integer; number of variables to be returned. If zero for \code{'free'} format, reads all values on a single line and does not check if n values are read or empty lines, '*' or '/' are present.
#' @param width integer; length of a single variable. Only used when format is \code{'fixed'}.  
#' @param nlay integer; number of layers for which values are to be read. Only use when a 1D(NLAY) variable is read which may be specified on multiple lines. Only used when format is \code{'fixed'}.
#' @param character logical; should a character vector be returned. Prevents conversion from character names to numeric. Defaults to FALSE. Useful if only characters are present on the line.
#' @param format character, either \code{'free'} or \code{'fixed'}. When 'fixed', reads character fields of length 'width' and converts to numeric. Empty fields are set to zero.
#' @param ... ignored
#' @return vector with values
#' @keywords internal
rmti_parse_variables <- function(remaining_lines, n, width = 10, nlay = NULL, character = FALSE, format = 'fixed', ...) {
  if(format == 'free') {
    if(n == 0) {
      nLines <- 1
      variables <- rmti_remove_empty_strings(strsplit(rmti_remove_comments_end_of_line(remaining_lines[1]),' |\t|,')[[1]])
    } else {
      n.cnt <- nLines <- 0
      end <- FALSE
      variables <- vector(mode = 'character')
      while(n.cnt < n && !end) {
        nLines <- nLines + 1
        values_ch <- rmti_remove_empty_strings(strsplit(paste(remaining_lines[nLines],collapse='\n'),' |\t|,')[[1]])
        
        # terminate if / is encountered
        slash <- grepl('/', values_ch)
        if(any(slash)) {
          last_value <- which(slash)[1]
          values_ch[last_value] <- sub('/.*', '', values_ch[last_value])
          values_ch <- values_ch[1:last_value]
          end <- TRUE
        }
        
        values_ch <- strsplit(values_ch, '\\*')
        values_ch <- lapply(values_ch, function(i) rmti_ifelse0(length(i) > 1, rep(i[[2]], as.numeric(i[[1]])), i))
        variables <- c(variables, unlist(values_ch))
        
        n.cnt <- length(variables)
      }
      variables <- variables[1:n]
    }
    if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
    return(list(variables=variables,remaining_lines=remaining_lines[-c(1:nLines)]))
    
  } else if(format == 'fixed') { # every value has 'width' characters; empty values are zero
    variables <- (unlist(lapply(seq(1,nchar(remaining_lines[1]), by=width), 
                                function(i) paste0(strsplit(rmti_remove_comments_end_of_line(remaining_lines[1]),'')[[1]][i:(i+min(width, nchar(remaining_lines[1])-i+1)-1)], collapse=''))))
    variables <- lapply(strsplit(variables, " |\t"), rmti_remove_empty_strings)
    variables[which(lengths(variables)==0)] <-  0 # empty values are set to 0
    variables <- unlist(variables)
    if(!is.null(nlay)) {
      while(length(variables) < nlay) { 
        remaining_lines <- remaining_lines[-1]
        variables <- append(variables, rmti_remove_empty_strings(strsplit(rmti_remove_comments_end_of_line(remaining_lines[1]),' |\t|,')[[1]]))
      }
    } else if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) { # convert to numeric
      variables <- as.numeric(variables)
      if(length(variables) < n) variables <- c(variables, rep(0, n - length(variables))) # append 0's if values are missing
    } else { # remain as character
      if(length(variables) < n) variables <- c(variables, rep('0', n - length(variables))) # append 0's if values are missing
    }
    return(list(variables=variables,remaining_lines=remaining_lines[-1]))
  }
}

#' Remove comments at the end of a string
#' @param line A string.
#' @return The string, without the commented part.
#' @keywords internal
rmti_remove_comments_end_of_line <- function(line) {
  if(grepl('!',line)) return(substr(line,1,regexpr('!',line)-1))
  else return(line)
}

#' Remove empty elements from a vector of strings.
#' @param vector_of_strings Vector of strings.
#' @return Vector of strings without the empty items.
#' @keywords internal
rmti_remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}

#' Write MT3DMS array
#' Internal function used in the rmt_write_* functions for writing array datasets
#' @param array array to write
#' @param file pathname to the file to write the array to
#' @param mf_style logical, should MODFLOW-style array headers be used (i.e. INTERNAL, EXTERNAL, OPEN/CLOSE, ...) ? Defaults to FALSE
#' @param format either 'free' (iread = 103, i.e. FORTRAN free format) or 'fixed' (iread = 100 using FORTRAN format 10G11.4). In both cases, iread = 0 when the array only contains 1 unique value. Defaults to 'free'. 
#' @param cnstnt numeric constant to add to the array header which acts as a multiplier for the array values in MODFLOW. Default to 1
#' @param iprn iprn code to add to array header. Defaults to -1
#' @param append logical; should array be appended to the file. Defaults to TRUE
#' @param ... passed to \code{\link{RMODFLOW:::rmfi_write_array}} when mf_style is TRUE
#' @return NULL
#' @keywords internal
rmti_write_array <- function(array, file, mf_style = FALSE, format = 'free', cnstnt=1, iprn=-1, append=TRUE, ...) {
  

  if(mf_style) {
    RMODFLOW:::rmfi_write_array(array = array, file = file, cnstnt = cnstnt, iprn = iprn, append = append, ...)
  } else {
    
    # MT3DMS
    # only iread 0 & 100 & 103 supported (CONSTANT & INTERNAL-ARRAY fixed and free format)
    # format: (10G11.4)
    
    fmt <- '(10G11.4)'
    nPerLine <- 10
    width <- 11
    decimals <- 4
    iprn <- as.integer(iprn)
    
    if(is.null(dim(array))) {
      if(prod(c(array)[1] == c(array))==1) {
        rmti_write_variables(0L, cnstnt * c(array)[1], file = file, append = append, width = 10)
      } else {
        
        if(format == 'free') {
          rmti_write_variables(103L, cnstnt, '(free)', iprn, file = file, append = append, width = c(10, 10, 20, 10))
          cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=append)     
        } else if(format == 'fixed') {
          rmti_write_variables(100L, cnstnt, fmt, iprn, file = file, append = append, width = c(10, 10, 20, 10))
          
          n <- length(c(array))
          nLines <- n %/% nPerLine
          remainder <- n %% nPerLine
          if(remainder > 0) nLines <- nLines + 1
          
          if(n > nPerLine) {
            formatted_array <- t(matrix('', nrow = nLines, ncol = nPerLine))
            formatted_array[1:n] <- formatC(c(array), format = 'g', digits = 4, width = 11)
            formatted_array <- t(formatted_array)
          } else {
            formatted_array <- formatC(c(array), format = 'g', digits = 4, width = 11)
          }
          write.table(formatted_array, file=file, append=append, sep='', col.names=FALSE, row.names=FALSE, quote = FALSE) 
          
        }
      }
    } else if(length(dim(array))==2) {
      if(prod(c(array)[1] == c(array))==1) {
        rmti_write_variables(0L, cnstnt * c(array)[1], file = file, append = append, width = 10)
      } else {
        
        if(format == 'free') {
          rmti_write_variables(103L, cnstnt, '(free)', iprn, file = file, append = append, width = c(10, 10, 20, 10))
          if(dim(array)[1] == 1) {
            cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=append)
          } else {
            write.table(array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
          }
        } else if(format == 'fixed') {
          rmti_write_variables(100L, cnstnt, fmt, iprn, file = file, append = append, width = c(10, 10, 20, 10))
          
          n <- length(c(array))
          nLines <- n %/% nPerLine
          remainder <- n %% nPerLine
          if(remainder > 0) nLines <- nLines + 1
          
          if(n > nPerLine) {
            formatted_array <- t(matrix('', nrow = nLines, ncol = nPerLine))
            formatted_array[1:n] <- formatC(c(array), format = 'g', digits = 4, width = 11)
            formatted_array <- t(formatted_array)
          } else {
            formatted_array <- formatC(c(array), format = 'g', digits = 4, width = 11)
          }
          write.table(formatted_array, file=file, append=append, sep='', col.names=FALSE, row.names=FALSE, quote = FALSE) 
          
        }
      }
    } else {
      for(i in 1:dim(array)[3])
      {
        if(prod(c(array[,,i])[1] == c(array[,,i]))==1) {
          rmti_write_variables(0L, cnstnt * c(array[,,i])[1], file = file, append = ifelse(i == 1, append, TRUE), width = 10)
        } else {
          
          if(format == 'free') {
            rmti_write_variables(103L, cnstnt, '(free)', iprn, file = file, append = ifelse(i == 1, append, TRUE), width = c(10, 10, 20, 10))
            if(dim(array)[1] == 1) {
              cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=ifelse(i == 1, append, TRUE))
            } else {
              write.table(array[,,i], file=file, append=ifelse(i == 1, append, TRUE), sep=' ', col.names=FALSE, row.names=FALSE)       
            }
          } else if(format == 'fixed') {
            rmti_write_variables(100L, cnstnt, fmt, iprn, file = file, append = ifelse(i == 1, append, TRUE), width = c(10, 10, 20, 10))
            
            n <- length(c(array[,,i]))
            nLines <- length(c(array[,,i])) %/% nPerLine
            remainder <- length(c(array[,,i])) %% nPerLine
            if(remainder > 0) nLines <- nLines + 1
            
            if(length(c(array[,,i])) > nPerLine) {
              formatted_array <- t(matrix('', nrow = nLines, ncol = nPerLine))
              formatted_array[1:n] <- formatC(c(array[,,i]), format = 'g', digits = 4, width = 11)
              formatted_array <- t(formatted_array)
            } else {
              formatted_array <- formatC(c(array[,,i]), format = 'g', digits = 4, width = 11)
            }
            write.table(formatted_array, file=file, append=ifelse(i == 1, append, TRUE), sep='', col.names=FALSE, row.names=FALSE, quote = FALSE) 
            
          }
        }
      }
    }
    
  }
}

#' Write MT3DMS variables
#' Internal function used in the rmt_write_* functions for writing single line datasets
#' @param format either \code{'fixed'} or \code{'free'}. Fixed format assumes fixed width character spaces for each value as determined by the width argument
#' @param width numeric vector with the character widths for each variable. If a single value, it is repeated.
#' @param integer logical; should all values be converted to integers? MT3D does not allow for exponents in integer values
#' @param iprn ignored
#' @return NULL
#' @keywords internal
rmti_write_variables <- function(..., file, append=TRUE, width = 10, format = 'fixed', integer = FALSE, iprn = -1) {
  
  arg <- list(...)
  arg <- arg[vapply(arg, function(i) all(nchar(i) > 0), TRUE)] # removes empty elements
  if(integer) arg <- lapply(arg, as.integer)
  
  # sets integers in proper format since Type is converted to double when vectorized
  if(format == 'free') {
    if(integer) {
      arg <- lapply(arg, formatC)
    } else {
      arg <- lapply(arg, as.character)
    }
    
    arg <- unlist(arg)
    cat(paste0(paste(arg, sep = ' ', collapse = ' '), '\n'), file=file, append=append)
  } else if(format == 'fixed') { 
    arg <- unlist(lapply(arg, as.list), recursive = FALSE)
    if(length(width) == 1) width <- rep(width, length(arg)) 
    arg <- lapply(1:length(arg), function(i) rmti_ifelse0(nchar(arg[[i]]) > width[i], formatC(arg[[i]], width = width[i]), paste0(paste0(rep(' ', width[i]-nchar(arg[[i]])), collapse = ''), as.character(arg[[i]]), collapse = '')))
    arg <- lapply(1:length(arg), function(i) paste0(strsplit(arg[[i]], '')[[1]][1:width[i]], collapse = ''))
    arg <- unlist(arg)
    cat(paste0(paste0(arg, collapse=''), '\n'), file=file, append=append)
  }
}
