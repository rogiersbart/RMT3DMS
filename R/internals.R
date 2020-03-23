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

#' Get an array specified by a  control record from the text lines analyzed in an \code{\link{RMT3DMS}} \code{rmt_read_*} function
#' @param remaining_lines lines to read the array from
#' @param nrow number of rows in the array
#' @param ncol number of columns in the array
#' @param nlay number of layers in the array that should be read
#' @param ndim optional; dimensions of the array to read
#' @param skip_header optional; should the control record be skipped
#' @param nam a \code{RMT3DMS} nam object. Required when reading external arrays
#' @param precision character: either \code{'single'} (default) or \code{'double'}. Denotes the precision of binary files
#' @param file pathname to the input file which is currently being read. Required when reading fixed-format or MODFLOW-style OPEN/CLOSE arrays
#' @param integer logical; does the binary array hold integer values. Might not work optimally.
#' @param ... ignored
#' @return A list containing the array and the remaining text of the MT3DMS input file
rmti_parse_array <- function(remaining_lines, nrow, ncol, nlay, ndim = NULL,
                             skip_header = FALSE, nam = NULL, precision = "single", file = NULL, integer = FALSE, ...) {
  
  header <- rmti_parse_variables(remaining_lines[1], n = 3, format = 'fixed')
  
  # MODFLOW-style free format control header
  if(header[1] %in% c('CONSTANT', 'INTERNAL', 'EXTERNAL', 'OPEN/CLOSE') || skip_header) {
    data_set <- RMODFLOW:::rmfi_parse_array(remaining_lines, nrow = nrow, ncol = ncol, nlay = nlay, ndim = ndim, 
                                            skip_header = skip_header, nam = nam, precision = precision, file = file, integer = integer, ...)
    data_set$array <- rmt_convert_rmf_to_rmt(data_set$array)
    
  } else {
    # MT3DMS fixed format control header
    
    # Initialize array object
    array <- array(dim=c(nrow,ncol,nlay))
    
    # Read array according to format type if there is anything to be read
    if(prod(dim(array))!=0)
    {
      for(k in 1:nlay) 
      { 
        fortranfmt <-  FALSE
        
        iread <-  as.numeric(header$variables[1])
        cnst <- as.numeric(header$variables[2])
        fmtin <-  trimws(as.character(header$variables[3]))
        
        if(iread == 0) {  # CONSTANT
          array[,,k] <- cnst
          nLines <- 1
          
        } else if(iread == 100) {  # INTERNAL-ARRAY
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
          nblock <- as.numeric(rmti_parse_variables(remaining_lines[1])$variables[1])
          nLines <- 1
          
          for(block in 1:nblock) {
            block_values <- as.numeric(rmti_parse_variables(remaining_lines[nLines + 1])$variables[1:5])
            array[block_values[1]:block_values[2], block_values[3]:block_values[4], k] <- block_values[5]
            nLines <- nLines + 1
          }
          array[,,k] <- array[,,k]*cnst
          
        } else if(iread == 102) {  # INTERNAL-ZONE
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          
          nzone <- as.numeric(rmti_parse_variables(remaining_lines[1])$variables[1])
          remaining_lines <- remaining_lines[-1]
          zv <- as.numeric(rmti_parse_variables(remaining_lines[2])$variables[1:nzone])
          remaining_lines <- remaining_lines[-1]
          zone <- array(dim = c(nrow, ncol))
          
          remaining_lines[1] <- paste(substring(remaining_lines[1], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' ')
          nPerLine <- length(lengths)
          nLines <- (ncol %/% nPerLine + ifelse((ncol %% nPerLine)==0, 0, 1))*nrow
          if(nLines > 1) remaining_lines[2:nLines] <- vapply(2:(nLines), function(i) paste(substring(remaining_lines[i], first = cumsum(lengths) - lengths + 1, last = cumsum(lengths)), collapse = ' '), 'text')
          zone <- cnst*matrix(as.numeric(rmti_remove_empty_strings(strsplit(paste(remaining_lines[1:nLines],collapse='\n'),' |,| ,|, |\t|\n| \n|\n ')[[1]])),nrow=nrow,ncol=ncol,byrow=TRUE)
          
          for(nz in 1:nzone) {
            zone[which(zone == nz)] <- zv[nz] 
          }
          array[,,k] <- zone*cnst
          
        } else if(iread == 103) {  # INTERNAL-LIST
          remaining_lines <- remaining_lines[-1] 
          if(cnst == 0) cnst <-  1.0
          
          n_final <- nrow*ncol
          n <- 0
          nLines <- 1
          values <- vector(mode = 'numeric')
          end <- FALSE
          
          while(n < n_final && !end) {
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
            
            nLines <- nLines + 1
            n <- length(values)
          }
          
          array[,,k] <- as.numeric(values[1:n_final])*cnst
          
          
        } else {  # EXTERNAL
          if(cnst == 0) cnst <-  1.0
          
          if(is.null(nam)) stop('Please supply a MT3DMS nam object when reading EXTERNAL arrays', call. = FALSE)
          
          fname <-  nam$fname[which(nam$nunit == iread)]
          direct <-  attr(nam, 'dir')
          absfile <-  paste(direct, fname, sep='/')
          
          # ASCII
          if(iread > 0) {
            if(!(toupper(fmtin) %in% c('(FREE)', 'FREE', '(BINARY)','BINARY'))) {
              lengths <- rmfi_fortran_format(fmtin)
              fortranfmt <-  TRUE
            }
            if(iread == nam$nunit[which(basename(nam$fname) == basename(file))]) { # read from current file
              
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
          if(is.null(attr(nam, as.character(iread)))) {
            attr(nam, as.character(iread)) <- nLines
          } else {
            attr(nam, as.character(iread)) <- attr(nam, as.character(iread)) + nLines
          }
          nLines <- 1
        }
        remaining_lines <- remaining_lines[-c(1:nLines)]
      }
    }
    
    # Set class of object (2darray; 3darray)
    if(is.null(ndim)) {
      if(nlay==1){
        if(ncol==1 || nrow==1) {
          array <- c(array(array,dim=nrow*ncol*nlay))
        } else {
          array <- rmt_create_array(array[,,1])
        }
      } else {
        array <- rmt_create_array(array)
      }
    } else if(ndim == 1) {
      array <- c(array(array,dim=nrow*ncol*nlay))
    } else if(ndim == 2) {
      array <- rmt_create_array(array[,,1], dim = c(nrow, ncol))
    } else if(ndim == 3) {
      array <- rmt_create_array(array, dim = c(nrow, ncol, nlay))
    }
    
    data_set <- list(array = array, remaining_lines = remaining_lines)
  }
  
  return(data_set)
}

#' @describeIn rmti_parse_array Deprecated function name
#' @export
read_mt3dms_array <- function(...) {
  .Deprecated(new = "rmti_parse_array", old = "read_mt3dms_array")
  rmti_parse_array(...)
}

#' Read mt3dms variables
#' @param n integer; number of variables to be returned. Only used when format is \code{'fixed'}.  
#' @param width integer; length of a single variable. Only used when format is \code{'fixed'}.  
#' @param nlay integer; number of layers for which values are to be read. Only use when a 1D(NLAY) variable is read which may be specified on multiple lines.
#' @param character logical; should a character vector be returned. Prevents conversion from character names to numeric. Defaults to FALSE. Useful if only characters are present on the line.
#' @param format character, either \code{'free'} or \code{'fixed'}. When 'fixed', reads character fields of length 'width' and converts to numeric. Empty fields are set to zero.
#' @param ... ignored
#' @keywords internal
rmti_parse_variables <- function(remaining_lines, n, width = 10, nlay = NULL, character = FALSE, format = 'fixed', ...) {
  if(format == 'free') {
    variables <- rmti_remove_empty_strings(strsplit(rmti_remove_comments_end_of_line(toupper(remaining_lines[1])),' |\t|,')[[1]])
    if(!is.null(nlay)) {
      while(length(variables) < nlay) { 
        remaining_lines <- remaining_lines[-1]
        variables <- append(variables, rmti_remove_empty_strings(strsplit(rmti_remove_comments_end_of_line(toupper(remaining_lines[1])),' |\t|,')[[1]]))
      }
    }
    if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) variables <- as.numeric(variables)
  } else if(format == 'fixed') { # every value has 'width' characters; empty values are zero
    variables <- (unlist(lapply(seq(1,nchar(remaining_lines[1]), by=width), 
                                function(i) paste0(strsplit(rmti_remove_comments_end_of_line(toupper(remaining_lines[1])),'')[[1]][i:(i+width-1)], collapse=''))))
    variables <- lapply(strsplit(variables, " |\t"), rmti_remove_empty_strings)
    variables[which(lengths(variables)==0)] <-  0 # empty values are set to 0
    variables <- unlist(variables)
    if(!is.null(nlay)) {
      while(length(variables) < nlay) { 
        remaining_lines <- remaining_lines[-1]
        variables <- append(variables, rmti_remove_empty_strings(strsplit(rmti_remove_comments_end_of_line(toupper(remaining_lines[1])),' |\t|,')[[1]]))
      }
    } else if(!character && !any(is.na(suppressWarnings(as.numeric(variables))))) {
      variables <- as.numeric(variables)
      if(length(variables) < n) variables <- c(variables, rep(0, n - length(variables))) # append 0's if values are missing
    }
  }
  return(list(variables=variables,remaining_lines=remaining_lines[-1]))
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
rmti_remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}

#' Write mt3dms array
#' Internal function used in the write_* functions for writing array datasets
#' @param mf_style logical, should MODFLOW-style array headers be used (i.e. INTERNAL, EXTERNAL, OPEN/CLOSE, ...) ? Defaults to FALSE
#' @param format either 'free' (iread = 103, i.e. FORTRAN free format) or 'fixed' (iread = 100 using FORTRAN format 10G11.4). In both cases, iread = 0 when the array only contains 1 unique value. Defaults to 'free'. 
#' @param ... passed to \code{\link{RMODFLOW:::rmfi_write_array}} when mf_style is TRUE
rmti_write_array <- function(array, file, mf_style = FALSE, format = 'free', cnstnt=1, iprn=-1, append=TRUE, ...) {
  
  if(mf_style) {
    RMODFLOW:::rmfi_write_array(array = array, file = file, cnstnt = cnstnt, iprn = iprn, append = append, ...)
  } else {
    
    # MT3DMS
    # only iread 0 & 100 supported (CONSTANT & INTERNAL-ARRAY)
    # format: (10G11.4)
    
    fmt <- '(10G11.4)'
    nPerLine <- 10
    width <- 11
    decimals <- 4
    
    if(is.null(dim(array))) {
      if(prod(c(array)[1] == c(array))==1) {
        rmti_write_variables(0, cnstnt * c(array)[1], file = file, append = append, width = 10)
      } else {
        
        if(format == 'free') {
          rmti_write_variables(103, cnstnt, '(free)', iprn, file = file, append = append, width = c(10, 10, 20, 10))
          cat(paste(paste(array, collapse=' '), '\n', sep=' '), file=file, append=append)     
        } else if(format == 'fixed') {
          rmti_write_variables(100, cnstnt, fmt, iprn, file = file, append = append, width = c(10, 10, 20, 10))
          
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
        rmti_write_variables(0, cnstnt * c(array)[1], file = file, append = append, width = 10)
      } else {
        
        if(format == 'free') {
          rmti_write_variables(103, cnstnt, '(free)', iprn, file = file, append = append, width = c(10, 10, 20, 10))
          if(dim(array)[1] == 1) {
            cat(paste0(paste(array, collapse=' '),'\n'), file=file, append=append)
          } else {
            write.table(array, file=file, append=append, sep=' ', col.names=FALSE, row.names=FALSE) 
          }
        } else if(format == 'fixed') {
          rmti_write_variables(100, cnstnt, fmt, iprn, file = file, append = append, width = c(10, 10, 20, 10))
          
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
          rmti_write_variables(0, cnstnt * c(array)[1], file = file, append = ifelse(i == 1, append, TRUE), width = 10)
        } else {
          
          if(format == 'free') {
            rmti_write_variables(103, cnstnt, '(free)', iprn, file = file, append = ifelse(i == 1, append, TRUE), width = c(10, 10, 20, 10))
            if(dim(array)[1] == 1) {
              cat(paste0(paste(array[,,i], collapse=' '),'\n'), file=file, append=ifelse(i == 1, append, TRUE))
            } else {
              write.table(array[,,i], file=file, append=ifelse(i == 1, append, TRUE), sep=' ', col.names=FALSE, row.names=FALSE)       
            }
          } else if(format == 'fixed') {
            rmti_write_variables(100, cnstnt, fmt, iprn, file = file, append = ifelse(i == 1, append, TRUE), width = c(10, 10, 20, 10))
            
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

#' Write mt3dms variables
#' Internal function used in the rmt_write_* functions for writing single line datasets
#' @param format either \code{'fixed'} or \code{'free'}. Fixed format assumes fixed width character spaces for each value as determined by the width argument
#' @param width numeric vector with the character widths for each variable. If a single value, it is repeated.
#' @keywords internal
rmti_write_variables <- function(..., file, append=TRUE, width = 10, format = 'fixed') {
  arg <- unlist(list(...))
  arg <- arg[nchar(arg) > 0] # removes empty elements
  arg <- vapply(arg, as.character, 'text')
  if(format == 'free') {
    cat(paste0(paste(arg, sep=' ',collapse=' '), '\n'), file=file, append=append)
  } else if(format == 'fixed') {
    if(length(width) == 1) width <- rep(width, length(arg)) 
    arg <- vapply(1:length(arg), function(i) formatC(arg[i], width = width[i]), 'text')
    cat(paste0(paste0(arg, collapse=''), '\n'), file=file, append=append)
  }
}
