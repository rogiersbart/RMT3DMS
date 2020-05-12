
#' Create an \code{RMT3DMS} dsp object
#'
#' \code{rmt_create_dsp} creates an \code{RMT3DMS} dsp object.
#'
#' @param btn \code{RMT3DMS} btn object
#' @param al 3D array with longitudinal dispersivities. Defaults to 10 for every cell.
#' @param trpt 1D vector with ratios of horizontal transverse to longitudinal dispersivities (TH/TL) for every layer. Defaults to 0.1 for every layer. 
#' @param trpv 1D vector with ratios of vertical transverse to longitudinal dispersivities (TV/TL) for every layer. Defaults to 0.01 for every layer. 
#' @param dmcoef either a 1D (nlay) (\code{multidiffusion = FALSE}) or a list of length \code{btn$mcomp} with 3D (\code{multidiffusion = TRUE}) array with the effective molecular diffusion coefficients. Defaults to 0 for all layers or cells (i.e. diffusion is neglected).
#' @param multidiffusion logical; if TRUE, \code{dmcoef} can be specified for each species, for each cell. Defaults to FALSE. MT3D-USGS only.
#' @param nocross logical; should cross-dispersion be disabled? Defaults to FALSE. MT3D-USGS only. 
#'
#' @return an object of class \code{dsp}
#' @export
#' @seealso \code{\link{rmt_read_dsp}}, \code{\link{rmt_write_dsp}}
#' @examples
#' btn <- rmt_create_btn(ncomp = 3)
#' rmt_create_dsp(btn)
#' rmt_create_dsp(btn, dmcoef = list(0, 0.1, 0.2), multidiffusion = TRUE)
rmt_create_dsp <- function(btn,
                           al = 10,
                           trpt = 0.1,
                           trpv = 0.01,
                           dmcoef = 0,
                           multidiffusion = FALSE,
                           nocross = FALSE) {
  
  dsp <- list()
  
  # optional keywords
  dsp$multidiffusion <- multidiffusion
  dsp$nocross <- nocross
  
  # data set 1
  dsp$al <- rmt_create_array(al, dim = c(btn$nrow, btn$ncol, btn$nlay))
  
  # data set 2
  dsp$trpt <- rmti_ifelse0(length(trpt) == 1, rep(trpt, btn$nlay), trpt)
  
  # data set 3
  dsp$trpv <- rmti_ifelse0(length(trpv) == 1, rep(trpv, btn$nlay), trpv)
  
  # data set 4
  if(!dsp$multidiffusion) {
    if(is.list(dmcoef) || !is.null(dim(dmcoef))) stop('dmcoef should be 1D when multidiffusion = FALSE', call. = FALSE)
    dsp$dmcoef <- rmti_ifelse0(length(dmcoef) == 1, rep(dmcoef, btn$nlay), dmcoef)
  } else {
    if(!is.list(dmcoef) && btn$mcomp > 1) stop('dmcoef should be a list with mcomp 3D arrays when multidiffusion = TRUE', call. = FALSE)
    dsp$dmcoef <- lapply(seq_len(btn$mcomp), function(i) rmt_create_array(dmcoef[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
  } 
  
  class(dsp) <- c('dsp', 'rmt_package')
  return(dsp)
  
}

#' Read an MT3DMS dispersion package file
#' 
#' \code{rmt_read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file filename; typically '*.dsp'
#' @param btn \code{RMT3DMS} btn object
#' @param ... optional arguments passed to \code{\link{rmti_parse_array}}
#' @return object of class dsp
#' @export
rmt_read_dsp <- function(file = {cat('Please select dsp file ...\n'); file.choose()},
                     btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                     ...) {
  
  dsp_lines <- readr::read_lines(file)
  dsp <- list()
  
  # options
  options <- rmti_parse_variables(dsp_lines, character = TRUE, format = 'free')
  options$variables <- sub('\\$', '', options$variables)
  dsp$multidiffusion <- 'MULTIDIFFUSION' %in% toupper(options$variables)
  dsp$nocross <- 'NOCROSS' %in% toupper(options$variables)
  
  if(sum(unlist(dsp)) > 0) dsp_lines <- options$remaining_lines
  
  # Data set 1
  data_set_1 <- rmti_parse_array(dsp_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, ...)
  dsp$al <- data_set_1$array
  dsp_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2
  data_set_2 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, ndim = 1, file = file, ...)
  dsp$trpt <- data_set_2$array
  dsp_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  # Data set 3
  data_set_3 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, ndim = 1, file = file, ...)
  dsp$trpv <- data_set_3$array
  dsp_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # Data set 4
  if(dsp$multidiffusion) {
    dsp$dmcoef <- list()
    for(comp in 1:btn$mcomp) {
      data_set_4 <- rmti_parse_array(dsp_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, ...)
      dsp$dmcoef[[comp]] <- rmt_create_array(data_set_4$array, solute = comp)
      dsp_lines <- data_set_4$remaining_lines
    }
    rm(data_set_4)
  } else {
    data_set_4 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, ndim = 1, file = file, ...)
    dsp$dmcoef <- data_set_4$array
    dsp_lines <- data_set_4$remaining_lines
    rm(data_set_4)
  }
  
  class(dsp) <- c('dsp','rmt_package')
  return(dsp)
}

#' Write an MT3DMS dispersion package file
#' 
#' @param dsp an \code{RMT3DMS} dsp object
#' @param file filename to write to; typically '*.dsp'
#' @param btn an \code{RMT3DMS} btn object
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmti_write_array}. 
#' @return \code{NULL}
#' @export
rmt_write_dsp <- function(dsp,
                      file = {cat('Please select dsp file to overwrite or provide new filename ...\n'); file.choose()},
                      btn = {cat('Please select corresponding btn file ...\n'); rmt_read_btn(file.choose())},
                      iprn=-1,
                      ...) {
  
  mf_style <- btn$modflowstylearrays
  
  if(dsp$multidiffusion || dsp$nocross) {
    rmti_write_variables('$', ifelse(dsp$multidiffusion, 'MULTIDIFFUSION', ''), ifelse(dsp$nocross, 'NOCROSS', ''), 
                         file = file, format = 'free', append = FALSE)
    # Data set 1
    rmti_write_array(dsp$al, file = file, iprn = iprn, mf_style = mf_style, ...)
    
  } else {
    
    # Data set 1
    rmti_write_array(dsp$al, file = file, iprn = iprn, append = FALSE, mf_style = mf_style, ...)
  }

  # Data set 2
  rmti_write_array(dsp$trpt, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 3
  rmti_write_array(dsp$trpv, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 4
  if(dsp$multidiffusion) {
    for(comp in 1:btn$mcomp) {
      rmti_write_array(dsp$dmcoef[[comp]], file = file, iprn = iprn, mf_style = mf_style, ...)
    }
  } else {
    rmti_write_array(dsp$dmcoef, file = file, iprn = iprn, mf_style = mf_style, ...)
  }
}
