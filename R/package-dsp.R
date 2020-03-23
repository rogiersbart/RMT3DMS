#' Read an MT3DMS dispersion package file
#' 
#' \code{rmt_read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
#' 
#' @param file filename; typically '*.dsp'
#' @param btn basic transport package file object
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
  data_set_1 <- rmti_parse_array(dsp_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
  dsp$al <- data_set_1$array
  dsp_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2
  data_set_2 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, file = file, ...)
  dsp$trpt <- data_set_2$array
  dsp_lines <- data_set_2$remaining_lines
  rm(data_set_2)
  
  # Data set 3
  data_set_3 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, file = file, ...)
  dsp$trpv <- data_set_3$array
  dsp_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # Data set 4
  if(dsp$multidiffusion) {
    dsp$dmcoef <- list()
    for(comp in 1:btn$ncomp) {
      data_set_4 <- rmti_parse_array(dsp_lines,btn$nrow,btn$ncol,btn$nlay, file = file, ...)
      dsp$dmcoef[[comp]] <- rmt_create_array(data_set_4$array, solute = comp)
      dsp_lines <- data_set_4$remaining_lines
    }
    rm(data_set_4)
  } else {
    data_set_4 <- rmti_parse_array(dsp_lines,1,btn$nlay,1, file = file, ...)
    dsp$dmcoef <- data_set_4$array
    dsp_lines <- data_set_4$remaining_lines
    rm(data_set_4)
  }
  
  class(dsp) <- c('dsp','rmt_package')
  return(dsp)
}

#' Write an MT3DMS dispersion package file
#' 
#' @param dsp an \code{\link{RMT3DMS}} dsp object
#' @param file filename to write to; typically '*.dsp'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmti_write_array}. 
#' @return \code{NULL}
#' @export
rmt_write_dsp <- function(dsp,
                      file = {cat('Please select dsp file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1,
                      ...) {
  
  if(dsp$multidiffusion || dsp$nocross) {
    rmti_write_variables('$', ifelse(dsp$multidiffusion, 'MULTIDIFFUSION', ''), ifelse(dsp$nocross, 'NOCROSS', ''), 
                         file = file, format = 'free', append = FALSE)
    # Data set 1
    rmti_write_array(dsp$al, file = file, iprn = iprn, ...)
    
  } else {
    
    # Data set 1
    rmti_write_array(dsp$al, file = file, iprn = iprn, append = FALSE, ...)
  }

  # Data set 2
  rmti_write_array(dsp$trpt, file = file, iprn = iprn, ...)
  
  # Data set 3
  rmti_write_array(dsp$trpv, file = file, iprn = iprn, ...)
  
  # Data set 4
  if(dsp$multidiffusion) {
    for(comp in 1:btn$ncomp) {
      rmti_write_array(dsp$dmcoef[[comp]], file = file, iprn = iprn, ...)
    }
  } else {
    rmti_write_array(dsp$dmcoef, file = file, iprn = iprn, ...)
  }
}
