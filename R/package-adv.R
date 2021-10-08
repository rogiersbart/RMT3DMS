
#' Create an \code{RMT3DMS} adv object
#'
#' \code{rmt_create_adv} creates an \code{RMT3DMS} adv object.
#'
#' @param mixelm integer code for the advection solution option (see details). Defaults to -1.
#' @param percel Courant number. Defaults to 1
#' @param mxpart maximum number of moving particles. Only used when mixelm = 1 or 3. Defaults to 1e6
#' @param nadvfd weighting scheme for implicit finite difference (mixelm = 0). 0 or 1 (default) indicates upstream weighting, 2 is central-in-space
#' @param itrack particle-tracking method: 1 (default) = first-order Euler, 2 = fourth-order Runge-Kutta, 3 = hybrid. Only used when mixelm = 1, 2 or 3.
#' @param wd concentration weighting factor used for operator splitting in particle tracking-based methods. Defaults to 0.5. Only used when mixelm = 1, 2 or 3.
#' @param dceps Relative Cell Concentration Gradient below which advective transport is negligble. Defaults to 1e-5. Only used when mixelm = 1 or 3.
#' @param nplane specifies type of initial placement of particles: random (0; default) or fixed pattern (> 0). Only used when mixelm = 1 or 3.
#' @param npl number of initial particles per cell at cells where Relative Cell Concentration Gradient is <= dceps. Defaults to 0. Only used when mixelm = 1 or 3.
#' @param nph number of initial particles per cell at cells where Relative Cell Concentration Gradient is > dceps. Defaults to 32. Only used when mixelm = 1 or 3.
#' @param npmin minimum number of particles allowed per cell. Defaults to 2. Only used when mixelm = 1 or 3.
#' @param npmax maximum number of particles allowed per cell. Defaults to \code{2 * nph}. Only used when mixelm = 1 or 3.
#' @param interp concentration interpolation method for MMOC scheme. Only a value of 1 is allowed (linear interpolation). Only used when mixelm = 2 or 3.
#' @param nlsink specifies type of initial placement of particles to approximate sink cells in MMOC: random (0) or fixed pattern (> 0). Defaults to \code{nplane}. Only used when mixelm = 2 or 3.
#' @param npsink number of particles used to approximate sink cells in MMOC. Defaults to \code{nph}. Only used when mixelm = 2 or 3.
#' @param dchmoc critical Relative Concentration Gradient for controlling selective use of MOC or MMOC in HMOC scheme. Defaults to 0.001. Only used when mixelm = 3.
#'
#' @details mixelm = 0: standard finite-difference method with upstream (nadvfd = 0 or 1) or central-in-space (nadvfd = 2) weighting.
#' mixelm = 1: forward-tracking method of characteristics (MOC)
#' mixelm = 2: backward-tracking modified method of characteristics (MMOC)
#' mixelm = 3: hybrid method of characteristics (HMOC) with MOC or MMOC automatically and dynamically selected
#' mixelm = -1: third-order TVD scheme (ULTIMATE)
#'
#' @return an object of class \code{adv}
#' @export
#' @seealso \code{\link{rmt_read_adv}}, \code{\link{rmt_write_adv}}
#' @examples
#' rmt_create_adv(percel = 0.75)
#' rmt_create_adv(mixelm = 3, nph = 16)
rmt_create_adv <- function(mixelm = -1,
                           percel = 1,
                           mxpart = 1e6,
                           nadvfd = 0,
                           itrack = 1,
                           wd = 0.5,
                           dceps = 1e-5,
                           nplane = 0,
                           npl = 0,
                           nph = 32,
                           npmin = 2,
                           npmax = 2 * nph,
                           interp = 1,
                           nlsink = nplane,
                           npsink = nph,
                           dchmoc = 0.001) {
  
  adv <- list()
  
  # data set 1
  adv$mixelm <- mixelm
  adv$percel <- percel
  adv$mxpart <- mxpart
  adv$nadvfd <- nadvfd
  
  # data set 2
  if(adv$mixelm %in% c(1,2,3)) {
    adv$itrack <- itrack
    adv$wd <- wd
  }
  
  # data set 3
  if(adv$mixelm %in% c(1,3)) {
    adv$dceps <- dceps
    adv$nplane <- nplane
    adv$npl <- npl
    adv$nph <- nph
    adv$npmin <- npmin
    adv$npmax <- npmax
  }
  
  # data set 4
  if(adv$mixelm %in% c(2,3)) {
    # adv$interp <- interp # only interp = 1 allowed
    adv$interp <- 1
    adv$nlsink <- nlsink
    adv$npsink <- npsink
  }
  
  # data set 5
  if(adv$mixelm == 3) {
    adv$dchmoc <- dchmoc
  }
  
  class(adv) <- c('adv', 'rmt_package')
  return(adv)
}

#' Read an MT3DMS advection package file
#' 
#' \code{rmt_read_adv} reads in an MT3DMS advection package file and returns it as an \code{\link{RMT3DMS}} adv object.
#' 
#' @param file filename; typically '*.adv'
#' @return object of class adv
#' @export
#' @seealso \code{\link{rmt_create_adv}}, \code{\link{rmt_write_adv}}
rmt_read_adv <- function(file = {cat('Please select adv file ...\n'); file.choose()}) {
  
  adv_lines <- readr::read_lines(file, lazy = FALSE)
  adv <- list()
  
  # Data set 1
  data_set_1 <- rmti_parse_variables(adv_lines, n = 4)
  adv$mixelm <- as.numeric(data_set_1$variables[1])
  adv$percel <- as.numeric(data_set_1$variables[2])
  if(adv$mixelm %in% c(1,3)) adv$mxpart <- as.numeric(data_set_1$variables[3])
  if(adv$mixelm == 0) {
    if(is.na(suppressWarnings(as.numeric(data_set_1$variables[4])))) {
      adv$nadvfd <- 0
    } else {
      adv$nadvfd <- as.numeric(data_set_1$variables[4])
    }
  } 
  adv_lines <- data_set_1$remaining_lines
  rm(data_set_1)
  
  # Data set 2
  if(adv$mixelm %in% c(1,2,3)) {  
    data_set_2 <- rmti_parse_variables(adv_lines, n = 2)
    adv$itrack <- as.numeric(data_set_2$variables[1])
    adv$wd <- as.numeric(data_set_2$variables[2])
    adv_lines <- data_set_2$remaining_lines
    rm(data_set_2)
  }
  
  # Data set 3
  if(adv$mixelm %in% c(1,3)) {  
    data_set_3 <- rmti_parse_variables(adv_lines, n = 6)
    adv$dceps <- as.numeric(data_set_3$variables[1])
    adv$nplane <- as.numeric(data_set_3$variables[2])
    adv$npl <- as.numeric(data_set_3$variables[3])
    adv$nph <- as.numeric(data_set_3$variables[4])
    adv$npmin <- as.numeric(data_set_3$variables[5])
    adv$npmax <- as.numeric(data_set_3$variables[6])
    adv_lines <- data_set_3$remaining_lines
    rm(data_set_3)
  }
  
  # Data set 4
  if(adv$mixelm %in% c(2,3)) {  
    data_set_4 <- rmti_parse_variables(adv_lines, n = 3)
    adv$interp <- as.numeric(data_set_4$variables[1])
    adv$nlsink <- as.numeric(data_set_4$variables[2])
    adv$npsink <- as.numeric(data_set_4$variables[3])
    adv_lines <- data_set_4$remaining_lines
    rm(data_set_4)
  }
  
  # Data set 5
  if(adv$mixelm == 3) {  
    data_set_5 <- rmti_parse_variables(adv_lines, n = 1)
    adv$dchmoc <- as.numeric(data_set_5$variables[1])
    adv_lines <- data_set_5$remaining_lines
  }
  
  class(adv) <- c('adv','rmt_package')
  return(adv)
}

#' Write an MT3DMS advection package file
#' 
#' @param adv an \code{RMT3DMS} adv object
#' @param file filename to write to; typically '*.adv'
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmt_read_adv}}, \code{\link{rmt_create_adv}}
rmt_write_adv <- function(adv,
                      file = {cat('Please select adv file to overwrite or provide new filename ...\n'); file.choose()}) {
  
  # Data set 1
  rmti_write_variables(as.integer(adv$mixelm), adv$percel, ifelse(adv$mixelm %in% c(1,3), as.integer(adv$mxpart), 0), ifelse(adv$mixelm == 0, as.integer(adv$nadvfd), ''), file = file, append = FALSE)
  
  # Data set 2
  if(adv$mixelm %in% c(1,2,3)) {  
    rmti_write_variables(as.integer(adv$itrack), adv$wd, file = file)
  }
  
  # Data set 3
  if(adv$mixelm %in% c(1,3)) {  
    rmti_write_variables(adv$dceps, as.integer(adv$nplane), as.integer(adv$npl), as.integer(adv$nph), as.integer(adv$npmin), as.integer(adv$npmax), file = file)
  }
  
  # Data set 4
  if(adv$mixelm %in% c(2,3)) {  
    rmti_write_variables(adv$interp, adv$nlsink, adv$npsink, file = file, integer = TRUE)
  }
  
  # Data set 5
  if(adv$mixelm == 3) {  
    rmti_write_variables(adv$dchmoc, file = file)
  }  
}
