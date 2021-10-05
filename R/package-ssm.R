
#' Create an \code{RMT3DMS} ssm object
#'
#' \code{rmt_create_ssm} creates an \code{RMT3DMS} ssm object.
#' 
#' @param ... one or more \code{rmt_list} objects or a single list containing them. These represent point sources/sinks.
#' @param btn \code{RMT3DMS} btn object
#' @param crch either a single value (used for all species in a multi-species simulation), a single \code{rmt_2d_array} or a list of \code{rmt_2d_arrays}. For multi-species simulations, a list of single values (one for each species) can also be entered. Represents the concentrations of the recharge flux from the flow model.
#' @param cevt either a single value (used for all species in a multi-species simulation), a single \code{rmt_2d_array} or a list of \code{rmt_2d_arrays}. For multi-species simulations, a list of single values (one for each species) can also be entered. Represents the concentrations of the evapotranspiration flux from the flow model.
#' @param mxss maximum number of all point sources and sinks in the flow model. Defaults to the number of cells. See details.
#' @param issgout unit number for an optional output file to save calculated flux-averaged concentrations at multi-node wells. Defaults to NULL. MT3D-USGS only.
#' @details The default value for \code{mxss} is likely to be much larger than the actual number of point sources/sinks. In order to preserve more computer memory, this value can be set to a more appropriate number. 
#'          All \code{rmt_2d_arrays} should have their kper and solute arguments set through \code{\link{rmt_create_array}}. There can only be one active \code{crch & cevt} \code{rmt_2d_array} per stress-period per species.
#'          If a negative concentration value/array for \code{crch} or \code{cevt} is specified for the first stress-period, concentrations of positive fluxes are set to zero and concentrations of a negative fluxes to the aquifer concentration for all species. If no other values/arrays are specified, this is repeated for all following stress-periods.
#'          If a flux is active in the flow simulation, values for the corresponding concentration fluxes must be supplied. E.g. if evt is used in the flow simulation, values for cevt must be supplied even if no concentrations are associated with this flux. In the latter case, supply negative concentration values.
#' @return an object of class \code{ssm}
#' @export
#' @seealso \code{\link{rmt_read_ssm}}, \code{\link{rmt_write_ssm}}
#' @examples
#' btn <- rmt_create_btn()
#' wel <- rmt_create_list(data.frame(i = 5, j = 4:5, k = 3, css1 = 6.45, css2 = 47, itype = 2), kper = 1)
#' rmt_create_ssm(wel, crch = 0, btn = btn)
#' 
#' btn <- rmt_create_btn(nper = 3, ncomp = 2)
#' wel_df <- data.frame(i = 5, j = 4:5, k = 3, css1 = 6.45, css2 = 47, itype = 2)
#' wel <- rmt_create_list(wel_df, kper = c(1,3))
#' wel2_df <- wel_df
#' wel2_df$css1 <- 0
#' wel2 <- rmt_create_list(wel2_df, kper = 2)
#' riv <- rmt_create_list(data.frame(i = 1, j = 1:10, k = 1, css1 = 12, css2 = 0, itype = 4), kper = c(1,2))
#' evt <- list(rmt_create_array(0, dim = c(btn$nrow, btn$ncol), solute = c(1,2), kper = c(1,3)),
#'             rmt_create_array(12, dim=c(btn$nrow, btn$ncol), solute = 1, kper = 2),
#'             rmt_create_array(0, dim=c(btn$nrow, btn$ncol), solute = 2, kper = 2))
#' rmt_create_ssm(wel, wel2, riv, crch = list(0, 2), cevt = evt, btn = btn)
#' 
#' rmt_create_ssm(cevt = -1, btn = btn)
#' 
rmt_create_ssm <- function(..., btn, crch = NULL, cevt = NULL, mxss = prod(btn$nrow, btn$ncol, btn$nlay), issgout = NULL) {
  
  ssm <- list()
  
  arg <- list(...)
  # if arg is nested list, unnest
  if(length(arg) == 1 && inherits(arg[[1]], 'list')) arg <- arg[[1]]
  if(length(arg) == 1 && is.null(arg[[1]])) arg <- NULL
  
  # get btn if in arg
  if(missing(btn)) {
    btn_arg <- vapply(arg, function(i) inherits(i, 'btn'), TRUE)
    if(sum(btn_arg) != 1) stop('Please supply a btn argument', call. = FALSE)
    btn <- arg[[which(btn_arg)]]
    arg <- arg[-which(btn_arg)]
  }
  
  # data set 1
  ssm$dimensions <- list()
  ssm$dimensions$ds1 <- structure(rep(FALSE, 8), names = c('fwel', 'fdrn', 'frch', 'fevt', 'friv', 'fghb', 'fuzf', 'fnew'))

  # data set 2
  ssm$dimensions$mxss <- mxss
  if(!is.null(issgout)) ssm$issgout <- issgout
  
  # data set 3
  if(!is.null(crch)) {
    # either a single value or single array, a list with single values, or a list with rmt_arrays
    # if single value, create array
    # if list, multiple entries represent solutes
    # if list entries are rmt_2d_arrays, solute attribute defines solute
    
    if(is.list(crch)) {
      rmt <- sum(vapply(crch, function(i) inherits(i, 'rmt_2d_array'), TRUE))
      if(rmt == 0) {
        # list with single values
        if(length(crch) != btn$ncomp) stop('Please supply a crch value for each species', call. = FALSE)
        crch <- lapply(seq_along(crch), function(i) rmt_create_array(crch[[i]], dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = i))
        
      } else if(rmt == length(crch)) {
        # rmt_2d_arrays
        if(any(vapply(crch, function(i) is.null(attr(i, 'kper')), TRUE))) {
          stop('Please make sure all rmt_2d_array objects have a kper attribute', call. = FALSE)
        } 
        
      } else {
        stop('If crch is a list, all elements must either be rmt_2d_arrays or single values', call. = FALSE)
      }
      
    } else if(!inherits(crch, 'rmt_2d_array') && length(crch) == 1) {
      crch <- list(rmt_create_array(crch, dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = 1:btn$ncomp))
    } else if(inherits(crch, 'rmt_2d_array')){
      if(is.null(attr(crch, 'solute')) || !identical(as.numeric(attr(crch, 'solute')), as.numeric(1:btn$ncomp))) {
        stop('Please make sure that all species are specified through rmt_2d_arrays. Perhaps reset the solute attribute?', call. = FALSE)
      }
      # crch <- rmt_create_array(crch, solute = 1:btn$ncomp)
      crch <- list(crch)
    } else {
      stop('crch must be either a single value, a single rmt_2d_array, a list of ncomp values or a list with rmt_2d_arrays', call. = FALSE)
    }
    
    # names
    names(crch) <- vapply(seq_along(crch), function(i) paste('crch', i, sep = '_'), 'text')
    
    # stress period data frame
    kper <- cbind(data.frame(kper = 1:btn$nper),
                  matrix(NA, btn$nper, btn$ncomp, dimnames = list(NULL, paste0('css', 1:btn$ncomp))))
    for(i in 1:length(crch)) {
      sp <- attr(crch[[i]], 'kper')
      icomp <- attr(crch[[i]], 'solute')
      error <- any(!is.na(kper[sp,icomp + 1,drop=FALSE]))
      if(error) stop('Multiple crch arrays specified for combinations of stress-period(s) ', paste0(p, collapse = ' '), ' and solute(s) ', paste0(icomp, collapse = ' '), call. = FALSE)
      kper[sp,icomp + 1] <- names(crch)[i]
    }
    if(any(is.na(unlist(kper[,-1])))) stop('crch is not defined for all species in all stress-periods', call. = FALSE)
    
    ssm$crch <- list(crch = crch, kper = kper)
    ssm$dimensions$ds1['frch'] <- TRUE
  }
  
  # data set 5
  if(!is.null(cevt)) {
    # either a single value or single array, a list with single values, or a list with rmt_arrays
    # if single value, create array
    # if list, multiple entries represent solutes
    # if list entries are rmt_2d_arrays, solute attribute defines solute
    
    if(is.list(cevt)) {
      rmt <- sum(vapply(cevt, function(i) inherits(i, 'rmt_2d_array'), TRUE))
      if(rmt == 0) {
        # list with single values
        if(length(cevt) != btn$ncomp) stop('Please supply a cevt value for each species', call. = FALSE)
        cevt <- lapply(seq_along(cevt), function(i) rmt_create_array(cevt[[i]], dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = i))
        
      } else if(rmt == length(cevt)) {
        # rmt_2d_arrays
        if(any(vapply(cevt, function(i) is.null(attr(i, 'kper')), TRUE))) {
          stop('Please make sure all rmt_2d_array objects have a kper attribute', call. = FALSE)
        } 
        
      } else {
        stop('If cevt is a list, all elements must either be rmt_2d_arrays or single values', call. = FALSE)
      }
      
    } else if(!inherits(cevt, 'rmt_2d_array') && length(cevt) == 1) {
      cevt <- list(rmt_create_array(cevt, dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = 1:btn$ncomp))
    } else if(inherits(cevt, 'rmt_2d_array')){
      if(is.null(attr(cevt, 'solute')) || !identical(as.numeric(attr(cevt, 'solute')), as.numeric(1:btn$ncomp))) {
        stop('Please make sure that all species are specified through rmt_2d_arrays. Perhaps reset the solute attribute?', call. = FALSE)
      }
      # cevt <- rmt_create_array(cevt, solute = 1:btn$ncomp)
      cevt <- list(cevt)
    } else {
      stop('cevt must be either a single value, a single rmt_2d_array, a list of ncomp values or a list with rmt_2d_arrays', call. = FALSE)
    }
    
    # names
    names(cevt) <- vapply(seq_along(cevt), function(i) paste('cevt', i, sep = '_'), 'text')
    
    # stress period data frame
    kper <- cbind(data.frame(kper = 1:btn$nper),
                  matrix(NA, btn$nper, btn$ncomp, dimnames = list(NULL, paste0('css', 1:btn$ncomp))))
    for(i in 1:length(cevt)) {
      sp <- attr(cevt[[i]], 'kper')
      icomp <- attr(cevt[[i]], 'solute')
      error <- any(!is.na(kper[sp,icomp + 1,drop=FALSE]))
      if(error) stop('Multiple cevt arrays specified for combinations of stress-period(s) ', sp, ' and solute(s) ', icomp, call. = FALSE)
      kper[sp,icomp + 1] <- names(cevt)[i]
    }
    if(any(is.na(unlist(kper[,-1])))) stop('cevt is not defined for all species in all stress-periods', call. = FALSE)
    
    ssm$cevt <- list(cevt = cevt, kper = kper)
    ssm$dimensions$ds1['fevt'] <- TRUE
  }
  
  # data set 12
  if(length(arg) > 0) {
    errors <- vapply(arg, function(i) inherits(i, 'rmt_list') && !is.null(attr(i, 'kper')), TRUE)
    if(any(!errors)) stop('Please make sure all arguments are of class rmt_list with defined kper attributes', call. = FALSE)

    # initialize emtpy point lists for each itype
    itype <- rmtd_itype
    points <- structure(rep(list(NULL), nrow(itype)), names = itype$names)
    
    ## helper functions
    
    # set proper column order in points list
    clean_cols <- function(lst, nm) {
      css_cols <- grep('css', colnames(lst))
      if(length(css_cols) > btn$ncomp) warning('Number of active species in btn is ', btn$ncomp, ', but more species defined in rmf_lists. Only using the first ', btn$ncomp, call. = FALSE)
      if(length(css_cols) < btn$ncomp) stop('Number of species is less than number of active species in btn (', btn$ncomp, ')', call. = FALSE)
      css_cols <- css_cols[1:btn$ncomp]
      lst$name <- nm
      lst <- lst[,c('k', 'i', 'j', 'name', 'itype', colnames(lst)[css_cols])]
      return(lst)
    }
    
    # set data & kper data.frames
    set_ssm <- function(itype_lst) {
      
      names(itype_lst) <- paste('list', 1:length(itype_lst), sep = '_')
      
      # get kper attributes
      kper <- cbind(data.frame(kper = 1:btn$nper),
                    matrix(FALSE, btn$nper, length(unique(names(itype_lst))), dimnames = list(NULL, unique(names(itype_lst)))))
      for(i in 1:length(itype_lst)) {
        kper[names(itype_lst)[i]] <- c(1:btn$nper) %in% attr(itype_lst[[i]],'kper')
      }
      
      # clean cols
      lst <- lapply(seq_along(itype_lst), function(i) clean_cols(itype_lst[[i]], names(itype_lst)[i]))
      
      # rbind to create a single lst
      bind_lst <- do.call(rbind, lst)
      
      return(list(data = bind_lst, kper = kper))
    }
    
    # loop over all rmt_lists & split them based on itype, rename to itype names and add every itype list to corresponding points list
    for(i in 1:length(arg)) {
      lst <- arg[[i]]
      pnts <- split(lst, lst$itype)
      names(pnts) <- itype$names[match(names(pnts), itype$itype)]
      
      # loop if rmt_list splits into multiple itypes
      for(j in 1:length(pnts)) {
        nm <- names(pnts)[j]
        points[[nm]][[length(points[[nm]]) + 1]] <- pnts[[j]]
      }
    }
    
    points <- points[!vapply(points, is.null, TRUE)] # remove empty itypes
    points <- lapply(points, set_ssm) # set data & kper for each itype
    ssm$points <- points
    
    # set data set 1
    if('wel' %in% names(ssm$points)) ssm$dimensions$ds1['fwel'] <- TRUE
    if('drn' %in% names(ssm$points)) ssm$dimensions$ds1['fdrn'] <- TRUE
    if('riv' %in% names(ssm$points)) ssm$dimensions$ds1['friv'] <- TRUE
    if('ghb' %in% names(ssm$points)) ssm$dimensions$ds1['fghb'] <- TRUE
    if(any(c('str', 'lak', 'sfr', 'mnw') %in% names(ssm$points))) ssm$dimensions$ds1['fnew'] <- TRUE
    
  }

  class(ssm) <- c('ssm', 'rmt_package')
  return(ssm)
}


#' Read an MT3DMS sink & source mixing package file
#' 
#' \code{rmt_read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \link{RMT3DMS} ssm object.
#' 
#' @param file filename; typically '*.ssm'
#' @param btn \code{RMT3DMS} btn object
#' @param ftl path the to flow-transport link file. See details.
#' @param ftl_free logical; is the flow-transport link file written in free (formatted) format (TRUE) or binary (unformatted) (FALSE)? if NULL (default), it is guessed from reading \code{ftl}
#' @param ... additional arguments passed to \code{rmti_parse_array}
#' @details \code{ftl} must be supplied to ensure the correct flow terms are read.
#' @return object of class \code{ssm}
#' @export
#' @seealso \code{\link{rmt_create_ssm}}, \code{\link{rmt_write_ssm}}
#' @examples 
rmt_read_ssm <- function(file = {cat('Please select ssm file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                         ftl = {cat('Please select corresponding ftl file ...\n'); file.choose()},
                         ftl_free = NULL,
                         ...) {
  
  ssm_lines <- readr::read_lines(file, lazy = FALSE)

  # data set 1 is not compulsary. It can also be read from the ftl file. If not read from ftl file, not possible to know if uzf is active
  # rmt_read_ssm only needs to know if rch, evt or uzf are used
  # data set 1
  # if(is.null(ftl)) {
  #   data_set_1 <- rmti_parse_variables(ssm_lines, n = 10, width = 2)
  #   frch <- as.logical(data_set_1$variables[3])
  #   fevt <- as.logical(data_set_1$variables[4])
  #   fuzf <- FALSE # can only be specified in FTL
  #   ssm_lines <- data_set_1$remaining_lines
  #   rm(data_set_1)
  # } else {
    if(is.null(ftl_free)) {
      binary <- NULL
    } else {
      binary <- !ftl_free
    }
    ftl.l <- rmti_parse_ftl_header(ftl, binary = binary)
    frch <- ftl.l['frch']
    fevt <- ftl.l['fevt']
    fuzf <- ftl.l['fuzf']
    ssm_lines <- ssm_lines[-1]
  # }
  # UZF not yet supported
  if(fuzf) warning('UZF transport not yet supported. Skipping UZF input.', call. = FALSE)

  # data set 2
  data_set_2 <- rmti_parse_variables(ssm_lines, n = 2, width = 10)
  mxss <- as.numeric(data_set_2$variables[1])
  issgout <- ifelse(is.na(suppressWarnings(as.numeric(data_set_2$variables[2]))), 0, as.numeric(data_set_2$variables[2]))
  if(issgout == 0) issgout <- NULL
  ssm_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  pnt_ss <- rch <- evt <- uzf <- cgwet <- NULL
  
  # helper functions
  read_array_bc <- function(lines, inc, sp, array_list) {

    set_previous_kper <- function(kk, kper) {
      if(!is.null(attr(kk, 'kper')) && kper-1 %in% attr(kk, 'kper')) {
        attr(kk, 'kper') <- c(attr(kk, 'kper'), kper)
      }
      return(kk)
    }
    
    if(inc >=0) {
      for(icomp in 1:btn$ncomp) {
        data_set_a <- rmti_parse_array(lines, nrow = btn$nrow, ncol = btn$ncol, nlay = 1, ndim = 2, file = file, ...)
        carray <- rmt_create_array(data_set_a$array, kper = sp, solute = icomp)
        array_list[[length(array_list) + 1]] <- carray
        lines <- data_set_a$remaining_lines
      }
      rm(data_set_a)
    } else {
      if(sp == 1) {
        carray <- rmt_create_array(-1, dim = c(btn$nrow, btn$ncol), kper = sp, solute = 1:btn$ncomp)
        array_list[[length(array_list) + 1]] <- carray
      } else {
        # use previous sp
        array_list <- lapply(array_list, function(i) set_previous_kper(i, sp))
      }
    }
    return(list(array_list = array_list, remaining_lines = lines))
  }
  
  read_list_bc <- function(remaining_lines, nss, sp) {
    
    df <- matrix(nrow=nss, ncol= 4 + ifelse(btn$ncomp > 1, 1 + btn$ncomp, 1))
    # if lines is of length 1, readr will assume it's a file connection and error out
    lines <- remaining_lines[1:nss]
    if(nss == 1) lines <- c(lines, '')
    
    if(btn$ncomp > 1) {
      widths <- readr::fwf_widths(c(rep(10, 5), NA))
      cols <- do.call(readr::cols_only, as.list(c(rep('i', 3), rep('d', 2), 'c')))
      df <- as.data.frame(readr::read_fwf(I(lines), widths, col_types = cols))
      
      df <- replace(df, which(is.na(df), arr.ind = TRUE), 0)
      
      # handle multispecies columns which may be free format
      df[[ncol(df)]] <- gsub(',', ' ', df[[ncol(df)]])
      # cols2 <- do.call(readr::cols_only, as.list(rep('d', btn$ncomp)))
      # lc <- as.data.frame(readr::read_table2(df[[ncol(df)]], col_names = FALSE, col_types = cols2))
      lc <- as.numeric(rmti_parse_variables(df[[ncol(df)]], n = btn$ncomp, format = 'free', character = TRUE)$variables)
      df <- cbind(df[-ncol(df)], matrix(lc, nrow = 1))
      colnames(df) <- c('k', 'i', 'j', 'dummy', 'itype', paste0('css', 1:btn$ncomp))
      
    } else {
      widths <- readr::fwf_widths(c(rep(10, 5)))
      cols <- do.call(readr::cols_only, as.list(c(rep('i', 3), rep('d', 2))))
      df <- as.data.frame(readr::read_fwf(I(lines), widths, col_types = cols))
      
      df <- replace(df, which(is.na(df), arr.ind = TRUE), 0)
      colnames(df) <- c('k', 'i', 'j', 'css1', 'itype')
    }
    
    df <- df[1:nss,]
    lst <- rmt_create_list(df, kper = sp)
    remaining_lines <- remaining_lines[-c(1:nss)]
    return(list(lst = lst, remaining_lines = remaining_lines))
  }
  
  # loop over stress-periods
  for(sp in 1:btn$nper) {
    # data set 3
    if(frch) {
      data_set_3 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      incrch <- as.numeric(data_set_3$variables[1])
      ssm_lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
      # data set 4
      data_set_4 <- read_array_bc(ssm_lines, incrch, sp, rch)
      rch <- data_set_4$array_list
      ssm_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # data set 5
    if(fevt) {
      data_set_5 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      inevt <- as.numeric(data_set_5$variables[1])
      ssm_lines <- data_set_5$remaining_lines
      rm(data_set_5)
      
      # data set 6
      data_set_6 <- read_array_bc(ssm_lines, inevt, sp, evt)
      evt <- data_set_6$array_list
      ssm_lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # TODO data sets 7-10
    # uzf not yet supported
    # read but not used in rmt_create_ssm
    if(fuzf) {
      # data set 7
      data_set_7 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      incuzf <- as.numeric(data_set_7$variables[1])
      ssm_lines <- data_set_7$remaining_lines
      rm(data_set_7)
      
      # data set 8
      data_set_8 <- read_array_bc(ssm_lines, incuzf, sp, uzf)
      uzf <- data_set_8$array_list
      ssm_lines <- data_set_8$remaining_lines
      rm(data_set_8)
      
      # data set 9
      data_set_9 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      incgwet <- as.numeric(data_set_9$variables[1])
      ssm_lines <- data_set_9$remaining_lines
      rm(data_set_9)
      
      # data set 10
      data_set_10 <- read_array_bc(ssm_lines, incgwet, sp, cgwet)
      cgwet <- data_set_10$array_list
      ssm_lines <- data_set_10$remaining_lines
      rm(data_set_10)
    }
    
    # data set 11
    data_set_11 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
    nss <- as.numeric(data_set_11$variables[1])
    ssm_lines <- data_set_11$remaining_lines
    rm(data_set_11)
    
    # data set 12
    if(nss > 0) {
      data_set_12 <- read_list_bc(ssm_lines, nss, sp)
      pnt_ss[[length(pnt_ss) + 1]] <- data_set_12$lst
      ssm_lines <- data_set_12$remaining_lines
      rm(data_set_12)
    }
  }
  
  ssm <- rmt_create_ssm(pnt_ss, crch = rch, cevt = evt, btn = btn, mxss = mxss, issgout = issgout)
  return(ssm)
}

#' Write an MT3DMS Sink-Source Mixing Package file
#' 
#' \code{rmt_write_ssm} writes an MT3DMS sink & source mixing package file from a \link{RMT3DMS} ssm object.
#' 
#' @param ssm an \code{RMT3DMS} ssm object
#' @param file filename to write to; typically '*.ssm'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param additional arguments passed to \code{\link{rmti_write_array}}
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmt_create_ssm}}, \code{\link{rmt_read_ssm}}
#' @examples 
#' btn <- rmt_create_btn()
#' wel <- rmt_create_list(data.frame(i = 5, j = 4:5, k = 3, css1 = 6.45, css2 = 47, itype = 2), kper = 1)
#' ssm <- rmt_create_ssm(wel, crch = 0, btn = btn)
#' rmt_write_ssm(ssm, file = file.path(tempdir(), 'input.ssm'), btn = btn)
rmt_write_ssm <- function(ssm,
                          file = {cat('Please select ssm file to overwrite or provide new filename ...\n'); file.choose()},
                          btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                          iprn = -1,
                          ...) {
  
  mf_style <- btn$modflowstylearrays
  
  # data set 1
  rmti_write_variables(ifelse(ssm$dimensions$ds1, 'T', 'F'), file = file, width = 2, append = FALSE)
  
  # data set 2
  rmti_write_variables(ssm$dimensions$mxss, if(!is.null(ssm$issgout)) {ssm$issgout}, file = file, integer = TRUE)
  
  # function to see if current stress-period has same active arrays as previous stress-period
  check_prev <- function(kper, i) {
    df <- kper[c(i-1,i), -1, drop = FALSE]
    identical(c(df[2,]), c(df[1,]))
  }
  
  # function to obtain active point sources/sinks for current stress-period
  get_nss <- function(pnts, sp) {
    nms <- colnames(pnts$kper)[-1]
    names_act <- nms[which(unlist(pnts$kper[sp,-1]))]
    if(length(names_act) > 0) {
      df <- pnts$data[pnts$data$name %in% names_act,]
      css_nm <- colnames(df)[grep('css', colnames(df))]
      if(btn$ncomp == 1) {
        df <- df[,c('k','i','j', css_nm,'itype')]
      } else {
        df$dummy <- -888
        df <- df[,c('k','i','j','dummy','itype', css_nm)]
      }
      df <- df[,which(colnames(df) != 'name')]
    } else {
      df <- NULL
    }
    return(df)
  }
  
  for(i in 1:btn$nper) {
    
    # data set 3 & 4
    if(ssm$dimensions$ds1['frch']) {
      
      # check if incrch should be -1 for first sp
      if(i == 1) {
        names_first <- unlist(ssm$crch$kper[i,-1])
        negt <- any(vapply(ssm$crch$crch[names_first], function(r) all(c(r) < 0), TRUE))
      } else {
        negt <- FALSE
      }
      
      if(i > 1 && check_prev(ssm$crch$kper, i)) {
        incrch <- -1
      } else if(i == 1 && negt) {
        incrch <- -1
      } else {
        incrch <- 1
      } 
      rmti_write_variables(incrch, file = file, integer = TRUE)
      
      if(incrch >= 0) {
        for(icomp in 1:btn$ncomp) {
          nm <- ssm$crch$kper[i,icomp + 1]
          rmti_write_array(ssm$crch$crch[[nm]], file = file, iprn = iprn, mf_style = mf_style, ...)
        }
      }
    }
    
    if(ssm$dimensions$ds1['fevt']) {
      
      # check if incevt should be -1 for first sp
      if(i == 1) {
        names_first <- unlist(ssm$cevt$kper[i,-1])
        negt <- any(vapply(ssm$cevt$cevt[names_first], function(r) all(c(r) < 0), TRUE))
      } else {
        negt <- FALSE
      }
      
      if(i > 1 && check_prev(ssm$cevt$kper, i)) {
        incevt <- -1
      } else if(i == 1 && negt) {
        incevt <- -1
      } else {
        incevt <- 1
      } 
      rmti_write_variables(incevt, file = file, integer = TRUE)
      
      if(incevt >= 0) {
        for(icomp in 1:btn$ncomp) {
          nm <- ssm$cevt$kper[i,icomp + 1]
          rmti_write_array(ssm$cevt$cevt[[nm]], file = file, iprn = iprn, mf_style = mf_style, ...)
        }
      }
    }
    
    # data set 11
    if(!is.null(ssm$points)) {
      lst <- lapply(ssm$points, function(x) get_nss(x, i))
      df <- do.call(rbind, lst)
      nss <- ifelse(is.null(df), 0, nrow(df))
    } else {
      nss <- 0
    }
    
    rmti_write_variables(nss, file = file, integer = TRUE)
    if(nss > 0) {
      if(btn$ncomp == 1) {
        fmt <- paste0(c(rep('%10i', 3), '%10g', '%10i'), collapse = '')
      } else {
        fmt <- paste0(c(rep('%10i', 3), '%10g', '%10i', rep('%10g', btn$ncomp)), collapse = '')
      }
      dff <- do.call('sprintf', c(df, fmt))
      readr::write_lines(dff, file = file, append = TRUE)
    }

  }
}
