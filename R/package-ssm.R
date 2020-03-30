

rmt_create_ssm <- function(.., crch = NULL, cevt = NULL, btn, mxss = prod(btn$nrow, btn$ncol, btn$nlay), issgout = NULL) {
  
  ssm <- list()
  
  arg <- list(...)
  
  # get btn if in arg
  if(missing(btn)) {
    btn_arg <- vapply(arg, function(i) inherits(i, 'btn'), TRUE)
    if(sum(btn_arg) != 1) stop('Please supply a btn argument', call. = FALSE)
    btn <- arg[[which(btn_arg)]]
    arg <- arg[-which(btn_arg)]
  }
  
  # data set 1
  trnop <- structure(rep(FALSE, 7), names = c('fwel', 'fdrn', 'frch', 'fevt', 'friv', 'fghb', 'fnew'))
  ssm$trnop <- trnop
  
  # data set 2
  ssm$mxss <- mxss
  if(!is.null(issgout)) ssm$issgout <- issgout
  
  # data set 3
  if(!is.null(crch)) {
    # if single value, create array
    # if list, multiple entries represent solutes
    # if list entries are rmt_2d_arrays, solute attribute defines solute
    
    if(is.list(crch)) {
      crc <- lapply(seq_len(crch), 
                    function(i) rmti_ifelse0(inherits(crch[[i]], 'rmt_2d_array'), 
                                             crch[[i]],
                                             rmt_create_array(crch[[i]], dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = i)))
    } else if(!inherits(crch, 'rmt_2d_array') && length(crch) == 1) {
      crch <- rmt_create_array(crch, dim = c(btn$nrow, btn$ncol), kper = 1:btn$nper, solute = 1:btn$ncomp)
    }
    ssm$trnop['frch'] <- TRUE
  }
  
  # data set 5
  if(!is.null(cevt)) {
    ssm$trnop['fevt'] <- TRUE
  }
  
  # data set 12
  if(length(arg) > 0) {
    # TODO check if all are of class rmt_list & kper (& solute?) is supplied for all
    errors <- vapply(arg, function(i) inherits(i, 'rmt_list') && !is.null(attr(i, 'kper')), TRUE)
    if(any(!errors)) stop('Please make sure all arguments are of class rmt_list with defined kper attributes', call. = FALSE)
    kper_arg <- lapply(arg, function(i) attr(i, 'kper'))
    
    # TODO create kper data frame and set name column (see RMODFLOW code)
    pnts <- do.call(rbind, arg)
    pnts <- split(pnts, pnts$itype)
    itype <- rmti_itype()
    names(pnts) <- itype$names[as.numeric(names(pnt))]
    
    # set data set 1
    if('wel' %in% names(pnts)) ssm$trnop['fwel'] <- TRUE
    if('drn' %in% names(pnts)) ssm$trnop['drn'] <- TRUE
    if('riv' %in% names(pnts)) ssm$trnop['riv'] <- TRUE
    if('ghb' %in% names(pnts)) ssm$trnop['ghb'] <- TRUE
    if(c('str', 'lak', 'sfr', 'mnw') %in% names(pnts)) ssm$trnop['new'] <- TRUE
    
    # set nss
        
  }

  class(ssm) <- c('ssm', 'rmt_package')
  return(ssm)
}


#' Read an MT3DMS sink & source mixing package file
#' 
#' \code{rmt_read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \code{\link{RMT3DMS}} ssm object.
#' 
#' @param file filename; typically '*.ssm'
#' @param btn \code{RMT3DMS} btn object
#' @return object of class \code{ssm}
#' @export
rmt_read_ssm <- function(file = {cat('Please select ssm file ...\n'); file.choose()},
                         btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())}) {
  
  ssm_lines <- readr::read_lines(file)
  ssm <- list()
  
  # TODO this line is not compulsary. Find a way to still know which packages are present
  # data set 1
  data_set_1 <- rmti_parse_variables(ssm_lines, n = 10, width = 2)
  ssm$fwel <- as.logical(data_set_1$variables[1])
  ssm$fdrn <- as.logical(data_set_1$variables[2])
  ssm$frch <- as.logical(data_set_1$variables[3])
  ssm$fevt <- as.logical(data_set_1$variables[4])
  ssm$friv <- as.logical(data_set_1$variables[5])
  ssm$fghb <- as.logical(data_set_1$variables[6])
  ssm$fnew <- as.logical(data_set_1$variables[7:10])
  ssm_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # data set 2
  data_set_2 <- rmti_parse_variables(ssm_lines, n = 2, width = 10)
  ssm$mxss <- as.numeric(data_set_2$variables[1])
  if(length(data_set_2$variables) > 1 && !is.na(suppressWarnings(as.numeric(data_set_2$variables[2])))) {
    ssm$issgout <- as.numeric(data_set_2$variables[2])
  }
  ssm_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  pnt_ss <- rch <- evt <- NULL
  
  # helper functions
  read_array_bc <- function(lines, inc, sp, array_list) {
    # TODO set sink concentration equal to aquifer concentration
    if(inc >=0) {
      for(icomp in 1:btn$ncomp) {
        data_set_a <- rmti_parse_array(lines, nrow = btn$nrow, ncol = btn$ncol, nlay = 1, file = file, ...)
        carray <- rmt_create_array(data_set_a$array, kper = sp, solute = icomp)
        array_list <- c(array_list, carray)
        lines <- data_set_a$remaining_lines
      }
      rm(data_set_a)
    } else {
      if(sp == 1) {
        # TODO set to sconc for sink (negative rch/evt flux)
        carray <- rmt_create_array(0, kper = sp, solute = 1:btn$ncomp)
        array_list <- c(array_list, carray)
      } else {
        # use previous sp
        sps <- vapply(array_list, function(i) attr(i, 'kper') == sp - 1, TRUE)
        attr(array_list[rev(which(sps))[1]], 'kper') <- c(attr(array_list[rev(which(sps))[1]], 'kper'), sp)
      }
    }
    return(list(array_list = array_list, remaining_lines = lines))
  }
  
  read_list_bc <- function(lines, nss) {
    # TODO set sink concentration equal to aquifer concentration
    # TODO try RMODFLOW:::rmfi_parse_bc_list/rmfi_parse_list
    lst <- list()
    for(i in 1:nss) {
      data_set_a <- rmti_parse_variables(lines, n = c(3, 1, 1, if(btn$ncomp > 1) {btn$ncomp}), width = 10)
      k <- as.numeric(data_set_a$variables[1])
      i <- as.numeric(data_set_a$variables[2])
      j <- as.numeric(data_set_a$variables[3])
      css <- as.numeric(data_set_a$variables[4])
      itype <- as.numeric(data_set_a$variables[5])
      df <- data.frame(k = k, i = i, j = j, css = css, itype = itype)
      if(btn$ncomp > 1) {
        vars <- paste0(data_set_a$variables[6:length(data_set_a$variables)], collapse = ' ')
        cssm <- rmti_parse_variables(vars, format = 'free')$variables[1:btn$ncomp]
        df2 <- as.data.frame(as.list(cssm), stringsAsFactors = FALSE, col.names = paste0('cssm', 1:btn$ncomp))
        df <- cbind(df, df2)
      }
      lines <- data_set_a$remaining_lines
      lst <- c(lst, df)
    }
    lst <- do.call(rbind, lst)
    rm(data_set_a)
    return(list(lst = lst, remaining_lines = lines))
  }
  
  # loop over stress-periods
  for(sp in 1:btn$nper) {
    # data set 3
    if(ssm$frch) {
      data_set_3 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      incrch <- as.numeric(data_set_3$variables[1])
      ssm_lines <- data_set_3$remaining_lines
      rm(data_set_3)
      
      # data set 4
      data_set_4 <- read_array_bc(ssm_lines, incrch, sp, rch)
      rch <- c(rch, data_set_4$array_list)
      ssm_lines <- data_set_4$remaining_lines
      rm(data_set_4)
    }
    
    # data set 5
    if(ssm$fevt) {
      data_set_5 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
      inevt <- as.numeric(data_set_5$variables[1])
      ssm_lines <- data_set_5$remaining_lines
      rm(data_set_5)
      
      # data set 6
      data_set_6 <- read_array_bc(ssm_lines, inevt, sp, evt)
      evt <- c(evt, data_set_6$array_list)
      ssm_lines <- data_set_6$remaining_lines
      rm(data_set_6)
    }
    
    # TODO data sets 7-10
    # uzf not yet supported
    
    # data set 11
    data_set_11 <- rmti_parse_variables(ssm_lines, n = 1, width = 10)
    nss <- as.numeric(data_set_11$variables[1])
    ssm_lines <- data_set_11$remaining_lines
    rm(data_set_11)
    
    # data set 12
    if(nss > 0) {
      data_set_12 <- read_list_bc(ssm_lines, nss)
      pnt_ss <- c(pnt_ss, data_set_12$lst)
      ssm_lines <- data_set_12$remaining_lines
      rm(data_set_12)
    }
  }
  
  ssm <- rmt_create_ssm(pnt_ss, rch = rch, evt = evt, btn = btn, mxss = mxss, issgout = issgout)
  # class(ssm) <- c('ssm', 'rmt_package')
  return(ssm)
}

#' Write an MT3DMS Sink-Source Mixing Package file
#' 
#' @param ssm an \code{RMT3DMS} ssm object
#' @param file filename to write to; typically '*.ssm'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @return \code{NULL}
#' @export
rmt_write_ssm <- function(ssm,
                          file = {cat('Please select ssm file to overwrite or provide new filename ...\n'); file.choose()},
                          btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                          iprn=-1) {
  
  # Data set D1
  cat(paste0(c(as.character(factor(c(ssm$fwel,ssm$fdrn,ssm$frch,ssm$fevt,ssm$friv,ssm$fghb,ssm$fnew),levels=c(TRUE,FALSE),labels=c(' T',' F'))), '\n'),collapse=''), file=file)
  
  # Data set D2
  cat(paste0(c(prettyNum(c(ssm$mxss),width=10), '\n'),collapse=''), file=file, append=TRUE)
  
  for(stress_period in 1:btn$nper) {
    
    # Data set D3
    if(ssm$frch) {
      cat(paste0(c(prettyNum(c(ssm$incrch[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      
      # Data set D4
      if(ssm$frch & (ssm$incrch[stress_period] >= 0)) {
        for(i in 1:btn$ncomp) {
          cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
          if(btn$ncomp>1) write.table(ssm$crch[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
          if(btn$ncomp==1) write.table(ssm$crch[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
    
    # Data set D5
    if(ssm$fevt) {
      cat(paste0(c(prettyNum(c(ssm$incevt[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      
      # Data set D6
      if(ssm$fevt & (ssm$incevt[stress_period] >= 0)) {
        for(i in 1:btn$ncomp) {
          cat(paste('       103         1           (NOTUSED)', formatC(iprn,width=10), '\n', sep=''), file=file, append=TRUE)
          if(btn$ncomp>1) write.table(ssm$cevt[[stress_period]][,,i], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
          if(btn$ncomp==1) write.table(ssm$cevt[[stress_period]], file=file, append=TRUE, sep=' ', col.names=FALSE, row.names=FALSE)       
        }
      }
    }
    
    # Data set D7
    cat(paste0(c(prettyNum(c(ssm$nss[stress_period]),width=10), '\n'),collapse=''), file=file, append=TRUE)
    
    # Data set D8
    if(ssm$nss > 0) {
      for(i in 1:ssm$nss[stress_period]) {
        if(btn$ncomp > 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i],ssm$cssms[[stress_period]][[i]]),width=10), '\n'),collapse=''), file=file, append=TRUE)
        if(btn$ncomp == 1) cat(paste0(c(prettyNum(c(ssm$kss[[stress_period]][i],ssm$iss[[stress_period]][i],ssm$jss[[stress_period]][i],ssm$css[[stress_period]][i],ssm$itype[[stress_period]][i]),width=10), '\n'),collapse=''), file=file, append=TRUE)
      }
    }
  }
}
