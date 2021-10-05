
#' Create a \code{RMT3DMS} btn object
#'
#' \code{rmt_create_btn} creates a \code{RMT3DMS} btn object.
#'
#' @param nlay number of layers; defaults to 3
#' @param nrow number of rows; defaults to 10
#' @param ncol number of columns; defaults to 10 
#' @param nper number of stress-periods in the flow model; defaults to 1
#' @param ncomp total number of chemical species in the simulation; defaults to 1
#' @param mcomp total number of mobile species; defaults to \code{ncomp}
#' @param tunit character; time unit. Only for identification purposes. Defaults to 's'.
#' @param lunit character; length unit. Only for identification purposes. Defaults to 'm'.
#' @param munit character: mass unit. Only for identification purposes. Defaults to 'mg'.
#' @param trnop logicals denoting which packages are active. Not used by MT3DMS > 4.50 or by MT3D-USGS. Defaults to setting adv, dsp, ssm, rct & gcg active.
#' @param laycon vector indicating type of model layer: confined (0) or convertible (not 0; default for all layers).
#' @param delr vector of cell widths along rows; defaults to 100 for all columns
#' @param delc vector of cell widths along columns; defaults to 100 for all rows
#' @param htop matrix with the top elevation of layer 1; defaults to 0 for all cells
#' @param dz 3D array with the thicknesses of each cell. Defaults to 10 for all cells.
#' @param prsity 3D array with the (effective or mobile) porosity values for each cell. Defaults to 0.3 for all cells.
#' @param icbund 3D array with integer values for each cell specifying if solute transport is active (1), inactive (0) or if the cell is a constant-concentration (-1). Defaults to 1 for all cells.
#' @param sconc list with \code{ncomp} 3D arrays specifying the starting concentrations in each cell for each species. Defaults to 0 for all cells for all species.
#' @param cinact value for indicating an inactive concentration cell (icbund = 0). Defaults to -888.
#' @param thkmin decimal fraction of layer thickness below which the cell is considered inactive. Defaults to 0.01. If negative, the absolute value is used.
#' @param ifmtcn flag and printing code to determine if calculated concentration should be printed to the standard output text file. Defaults to 0 (no printing).
#' @param ifmtnp flag and printing code to determine if number of particles in each cell should be printed to the standard output text file. Defaults to 0 (no printing).
#' @param ifmtrf flag and printing code to determine if the model-calculated retardation factor should be printed to the standard output text file. Defaults to 0 (no printing).
#' @param ifmtdp flag and printing code to determine if the model-calculated distance-weighted dispersion coefficient should be printed to the standard output text file. Defaults to 0 (no printing).
#' @param savucn logical determining if concentration solution should be saved in default unformatted (binary) file named MT3Dnnn.UCN where nnn is the species index number. Defaults to TRUE.
#' @param nprs frequency of output. Ignored if \code{timprs} is supplied. If positive, \code{timpr} must be supplied. If negative, output is printed or saved whenever the number of transport steps is an even multiple of \code{nprs}. If zero, only print or save at the end of the simulation. Defaults to -1.
#' @param timprs vector with user-defined printing/saving times. Defaults to NULL.
#' @param obs optional data.frame with k, i and j columns indicating the cells for which species' concentrations will be saved in MT3Dnnn.OBS files (nnn indicating species index). Defaults to NULL. 
#' @param nprobs integer indicating how frequently concentrations at \code{obs} observation points should be saved. Concentrations are saved every \code{nprobs} step. Defaults to 1.
#' @param chkmas logical indicating whether a one-line summary of the mass balance should be saved to a default MT3Dnnn.MAS file where nnn is the species index. Defaults to TRUE.
#' @param nprmas frequency of saving the mass budget information to MT3Dnnn.MAS. Information is saved every \code{nprmas} step. Defaults to 1.
#' @param perlen vector of flow model stress-period lengths. Defaults to 1 for every stress-period.
#' @param nstp vector of flow model stress-period time steps. Defaults to 1 for every stress-period.
#' @param tsmult vector of successive flow model time step length multipliers. Defaults to 1 for every stress-period.
#' @param ssflag logical vector of length \code{nper} indicating if the steady-state transport option should be used. Defaults to FALSE for every stress-period.
#' @param tslngh list of length \code{nper} with time step length vectors. Defaults to 1 for every time step length. Only used when \code{tsmult > 0} for the current stress-period.
#' @param dt0 vector of length \code{nper} of initial transport step sizes within each flow time step. If zero (default) and the GCG solver is active, a model-calculated value based on the Courant number in the Advection package is used. 
#' @param mxstrn vector of length \code{nper} with the maximum number of transport steps allowed within one flow time step. Defaults to 50 for every stress-period.
#' @param ttsmult vector of length \code{nper} with transport step size multipliers within a single flow time step. Defaults to 1 for every stress-period.
#' @param ttsmax vector of length \code{nper}with maximum allowed transport step size when \code{ttsmult > 1}. Defaults to 0 for every stress-period (no limit).
#' @param modflowstylearrays logical; should MODFLOW-style arrays and array headers be used. Defaults to FALSE. MT3D-USGS only.
#' @param drycell logical; should mass transfer through dry cells be enabled when dry cells remain active in the flow simulation (e.g. with MODFLOW-NWT). Only available when mixelm = 0 or -1 in the Advection package. Defaults to FALSE. MT3D-USGS only.
#' @param legacy99storage logical; should MT3DMS flow storage computations be used. Defaults to FALSE. MT3D-USGS only.
#' @param ftlprint logical; should the contents of the flow-transport link file be printed to the standard output text file. Defaults to FALSE. MT3D-USGS only.
#' @param nowetdryprint logical; should printing of messages related to drying and re-wetting of model cells to the standard output text file be disabled? Defaults to FALSE. MT3D-USGS only.
#' @param omitdrycellbudget logical; should the printing of the mass budget of dry cells be disabled? Defaults to FALSE. MT3D-USGS only.
#' @param altwtsorb logical; use an alternative formulation to simulate adsorbed mass. Defaults to FALSE. MT3D-USGS only.
#' @param prj \code{RMODFLOW} prj object. Defaults to NULL.
#'
#' @return an object of class \code{btn}
#' @export
#' @seealso \code{\link{rmt_read_btn}}, \code{\link{rmt_write_btn}}
#' @examples
#' rmt_create_btn()
#' prsity <- array(rep(c(0.1, 0.01, 0.3), each = 100), dim = c(10, 10, 3))
#' rmt_create_btn(nper = 3, prsity = prsity)
#' 
#' # multi-species; sconc for species 1 is from rnorm, sconc for species 2 = 0.2
#' sconc1 <- rmt_create_array(rnorm(100, 0.1, sd = 0.01), dim = c(10, 10, 3))
#' rmt_create_btn(ncomp = 2, prsity = prsity, sconc = list(sconc1, 0.2))
rmt_create_btn <- function(nlay = 3,
                           nrow = 10,
                           ncol = 10,
                           nper = 1,
                           ncomp = 1,
                           mcomp = ncomp,
                           tunit = 's',
                           lunit = 'm',
                           munit = 'g',
                           trnop = c(rep(TRUE, 5), rep(FALSE, 5)),
                           laycon = rep(1, nlay),
                           delr = 100,
                           delc = 100,
                           htop = 0,
                           dz = 10,
                           prsity = 0.3,
                           icbund = 1,
                           sconc = as.list(rep(0, ncomp)),
                           cinact = -888,
                           thkmin = 0.01,
                           ifmtcn = 0,
                           ifmtnp = 0,
                           ifmtrf = 0,
                           ifmtdp = 0,
                           savucn = TRUE,
                           nprs = -1,
                           timprs = NULL,
                           obs = NULL,
                           nprobs = 1,
                           chkmas = TRUE,
                           nprmas = 1,
                           perlen = rep(1, nper),
                           nstp = rep(1, nper),
                           tsmult = rep(1, nper),
                           ssflag = rep(FALSE, nper),
                           tslngh = lapply(seq_len(nper), function(i) rep(1, nstp[i])),
                           dt0 = rep(0, nper),
                           mxstrn = rep(50, nper),
                           ttsmult = rep(1, nper),
                           ttsmax = rep(0, nper),
                           modflowstylearrays = FALSE,
                           drycell = FALSE,
                           legacy99storage = FALSE,
                           ftlprint = FALSE,
                           nowetdryprint = FALSE,
                           omitdrycellbudget = FALSE,
                           altwtsorb = FALSE,
                           prj = NULL) {
  
  btn <- list()
  
  # MT3D-USGS keywords
  btn$modflowstylearrays <- modflowstylearrays
  btn$drycell <- drycell
  btn$legacy99storage <- legacy99storage
  btn$ftlprint <- ftlprint
  btn$nowetdryprint <- nowetdryprint
  btn$omitdrycellbudget <- omitdrycellbudget
  btn$altwtsorb <- altwtsorb
  
  
  # data set 3
  btn$nlay <- nlay
  btn$nrow <- nrow
  btn$ncol <- ncol
  btn$nper <- nper
  btn$nper <- nper
  btn$ncomp <- ncomp
  btn$mcomp <- mcomp
  
  # data set 4
  btn$tunit <- tunit
  btn$lunit <- lunit
  btn$munit <- munit
  
  # data set 5
  btn$trnop <- trnop
  
  # data set 6
  btn$laycon <- rmti_ifelse0(length(laycon) == 1, rep(laycon, btn$nlay), laycon)
  
  # data set 7
  btn$delr <- rmti_ifelse0(length(delr) == 1, rep(delr, btn$ncol), delr)
  
  # data set 8
  btn$delc <- rmti_ifelse0(length(delc) == 1, rep(delc, btn$nrow), delc)
  
  # data set 9
  btn$htop <- rmt_create_array(htop, dim = c(btn$nrow, btn$ncol))
  
  # data set 10
  btn$dz <- rmt_create_array(dz, dim = c(btn$nrow, btn$ncol, btn$nlay))
  
  # data set 11
  btn$prsity <- rmt_create_array(prsity, dim = c(btn$nrow, btn$ncol, btn$nlay))
  
  # data set 12
  btn$icbund <- rmt_create_array(as.integer(icbund), dim = c(btn$nrow, btn$ncol, btn$nlay))
  
  # data set 13
  if(!is.list(sconc) && btn$ncomp == 1) {
    btn$sconc <- list(rmt_create_array(sconc, dim = c(btn$nrow, btn$ncol, btn$nlay)))
  } else {
    if(!is.list(sconc) && btn$ncomp > 1) stop('sconc should be a list with ncomp 3D arrays', call. = FALSE)
    btn$sconc <- lapply(seq_len(btn$ncomp), function(i) rmt_create_array(sconc[[i]], dim = c(btn$nrow, btn$ncol, btn$nlay)))
  }
  
  # data set 14
  btn$cinact <- cinact
  btn$thkmin <- thkmin
  
  # data set 15
  btn$ifmtcn <- ifmtcn
  btn$ifmtnp <- ifmtnp
  btn$ifmtrf <- ifmtrf
  btn$ifmtdp <- ifmtdp
  btn$savucn <- savucn
  
  # data set 16 - 17
  if(!is.null(timprs)) {
    btn$nprs <- length(timprs)
    btn$timprs <- timprs
  } else {
    if(nprs > 0) stop('Please supply a timprs argument when nprs > 0', call. = FALSE)
    btn$nprs <- nprs
  }
  
  # data set 18 & 19
  if(!is.null(obs)) {
    btn$obs <- obs[,c('k', 'i', 'j')]
    btn$nprobs <- nprobs
  }
  
  # data set 20
  btn$chkmas <- chkmas
  btn$nprmas <- nprmas
  
  # data set 21
  btn$perlen <- rmti_ifelse0(length(perlen) == 1, rep(perlen, btn$nper), perlen)
  btn$nstp <- rmti_ifelse0(length(nstp) == 1, rep(nstp, btn$nper), nstp)
  btn$tsmult <- rmti_ifelse0(length(tsmult) == 1, rep(tsmult, btn$nper), tsmult)
  btn$ssflag <- rmti_ifelse0(length(ssflag) == 1, rep(ssflag, btn$nper), ssflag)
  
  # data set 22
  if(any(btn$tsmult <= 0)) btn$tslngh <- tslngh
  
  # data set 23
  btn$dt0 <- rmti_ifelse0(length(dt0) == 1, rep(dt0, btn$nper), dt0)
  btn$mxstrn <- rmti_ifelse0(length(mxstrn) == 1, rep(mxstrn, btn$nper), mxstrn)
  btn$ttsmult <- rmti_ifelse0(length(ttsmult) == 1, rep(ttsmult, btn$nper), ttsmult)
  btn$ttsmax <- rmti_ifelse0(length(ttsmax) == 1, rep(ttsmax, btn$nper), ttsmax)
  
  class(btn) <- c('btn', 'rmt_package')
  btn <- RMODFLOW::rmf_set_prj(btn, prj)
  return(btn)
  
}

#' Read a MT3DMS basic transport package file
#' 
#' \code{rmt_read_btn} reads in a MT3DMS basic transport package file and returns it as a \link{RMT3DMS} btn object.
#' 
#' @param file filename; typically '*.btn'
#' @param ... optional arguments passed to \code{\link{rmti_parse_array}}
#' @return object of class btn
#' @export
#' @seealso \code{\link{rmt_create_btn}}, \code{\link{rmt_write_btn}}
rmt_read_btn <- function(file = {cat('Please select btn file ...\n'); file.choose()}, ...) {
  
  btn_lines <- readr::read_lines(file, lazy = FALSE)
  btn <- list()
  
  # Data set 1-2
  data_set_0 <- rmti_parse_comments(btn_lines, id = 1:2)
  comment(btn) <- data_set_0$comments
  btn_lines <- data_set_0$remaining_lines
  rm(data_set_0)
  
  # Data set 3
  data_set_3 <- rmti_parse_variables(btn_lines, n = 0, character = TRUE, format = 'free')
  
  # Options in MT3D-USGS
  btn$modflowstylearrays <- 'MODFLOWSTYLEARRAYS' %in% toupper(data_set_3$variables)
  btn$drycell <- 'DRYCELL' %in% toupper(data_set_3$variables)
  btn$legacy99storage <- 'LEGACY99STORAGE' %in% toupper(data_set_3$variables)
  btn$ftlprint <- 'FTLPRINT' %in% toupper(data_set_3$variables)
  btn$nowetdryprint <- 'NOWETDRYPRINT' %in% toupper(data_set_3$variables)
  btn$omitdrycellbudget <- 'OMITDRYCELLBUDGET' %in% toupper(data_set_3$variables)
  btn$altwtsorb <- 'ALTWTSORB' %in% toupper(data_set_3$variables)
  
  if(sum(unlist(btn)) > 0) btn_lines <- data_set_3$remaining_lines
  
  data_set_3 <- rmti_parse_variables(btn_lines, n = 6)
  btn$nlay <- as.numeric(data_set_3$variables[1])
  btn$nrow <- as.numeric(data_set_3$variables[2])
  btn$ncol <- as.numeric(data_set_3$variables[3])
  btn$nper <- as.numeric(data_set_3$variables[4])
  btn$ncomp <- as.numeric(data_set_3$variables[5])
  btn$mcomp <- as.numeric(data_set_3$variables[6])
  if(btn$ncomp < 1) btn$ncomp <- 1
  if(btn$mcomp < 1) btn$mcomp <- 1
  btn_lines <- data_set_3$remaining_lines
  rm(data_set_3)
  
  # Data set 4
  data_set_4 <- rmti_parse_variables(btn_lines, character = TRUE, n = 3, width = 4)
  btn$tunit <- data_set_4$variables[1]
  btn$lunit <- data_set_4$variables[2]
  btn$munit <- data_set_4$variables[3]
  btn_lines <- data_set_4$remaining_lines
  rm(data_set_4)
  
  # Data set 5
  data_set_5 <- rmti_parse_variables(btn_lines, n = 10, width = 2)
  btn$trnop <- as.logical(data_set_5$variables[1:10])
  btn$trnop <- ifelse(is.na(btn$trnop), FALSE, btn$trnop)
  btn_lines <- data_set_5$remaining_lines
  rm(data_set_5)
  
  # Data set 6
  data_set_6 <- rmti_parse_variables(btn_lines, nlay = btn$nlay, width = 2)
  btn$laycon <- as.numeric(data_set_6$variables[1:btn$nlay])
  btn_lines <- data_set_6$remaining_lines
  rm(data_set_6)
  
  # Data set 7
  data_set_7 <- rmti_parse_array(btn_lines,1,btn$ncol,1, ndim = 1, file = file, ...)
  btn$delr <- data_set_7$array
  btn_lines <- data_set_7$remaining_lines
  rm(data_set_7)
  
  # Data set 8
  data_set_8 <- rmti_parse_array(btn_lines,1,btn$nrow,1, ndim = 1, file = file, ...)
  btn$delc <- data_set_8$array
  btn_lines <- data_set_8$remaining_lines
  rm(data_set_8)
  
  # Data set 9
  data_set_9 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,1, ndim = 2, file = file, ...)
  btn_lines <- data_set_9$remaining_lines
  btn$htop <- data_set_9$array
  rm(data_set_9)
  
  # Data set 10
  data_set_10 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, ...)
  btn_lines <- data_set_10$remaining_lines
  btn$dz <- data_set_10$array
  rm(data_set_10)
  
  # Data set 11
  data_set_11 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, ...)
  btn_lines <- data_set_11$remaining_lines
  btn$prsity <- data_set_11$array
  rm(data_set_11)
  
  # Data set 12
  data_set_12 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, integer = TRUE, ...)
  btn_lines <- data_set_12$remaining_lines
  btn$icbund <- rmt_create_array(as.integer(data_set_12$array), dim = c(btn$nrow, btn$ncol, btn$nlay))
  rm(data_set_12)
  
  # Data set 13
  btn$sconc <- list()
  for(species in 1:btn$ncomp) {
    data_set_13 <- rmti_parse_array(btn_lines,btn$nrow,btn$ncol,btn$nlay, ndim = 3, file = file, ...)
    btn_lines <- data_set_13$remaining_lines
    btn$sconc[[species]] <- rmt_create_array(data_set_13$array, solute = species)
    rm(data_set_13)
  }
  
  # Data set 14
  data_set_14 <- rmti_parse_variables(btn_lines, n = 2, width = 10)
  btn$cinact <- as.numeric(data_set_14$variables[1])
  btn$thkmin <- as.numeric(data_set_14$variables[2])
  btn_lines <- data_set_14$remaining_lines
  rm(data_set_14)
  
  # Data set 15
  data_set_15 <- rmti_parse_variables(btn_lines, n = 5, width = 10, character = TRUE)
  btn$ifmtcn <- as.numeric(data_set_15$variables[1])
  btn$ifmtnp <- as.numeric(data_set_15$variables[2])
  btn$ifmtrf <- as.numeric(data_set_15$variables[3])
  btn$ifmtdp <- as.numeric(data_set_15$variables[4])
  btn$savucn <- as.logical(data_set_15$variables[5])
  btn_lines <- data_set_15$remaining_lines   
  rm(data_set_15)
  
  # Data set 16
  data_set_16 <- rmti_parse_variables(btn_lines, n = 1, width = 10)
  btn$nprs <- as.numeric(data_set_16$variables[1])
  btn_lines <- data_set_16$remaining_lines
  rm(data_set_16)
  
  # Data set 17
  if(btn$nprs > 0) {
    ntmp <- 0
    timprs <- NULL
    while(ntmp < btn$nprs) {
      n <- ifelse(btn$nprs - ntmp > 8, 8, btn$nprs - ntmp)
      data_set_17 <- rmti_parse_variables(btn_lines, n = n, width = 10)
      timprs <- append(timprs, as.numeric(data_set_17$variables[1:n]))
      btn_lines <- data_set_17$remaining_lines
      ntmp <- ntmp + n
    }
    btn$timprs <- timprs
    rm(data_set_17)
  }
  
  # Data set 18
  data_set_18 <- rmti_parse_variables(btn_lines, n = 2, width = 10)
  nobs <- as.numeric(data_set_18$variables[1])
  nprobs <- as.numeric(data_set_18$variables[2])
  btn_lines <- data_set_18$remaining_lines
  rm(data_set_18)
  
  # Data set 19
  if(nobs > 0) {
    # using readr (fast)
    # if lines is of length 1, readr will assume it's a file connection and error out
    lines <- btn_lines[1:nobs]
    if(nobs == 1) lines <- c(lines, '')    
    
    widths <- readr::fwf_widths(c(rep(10, 3)))
    cols <- do.call(readr::cols_only, as.list(rep('i', 3)))
    df <- as.data.frame(readr::read_fwf(I(lines), widths, col_types = cols))
    df <- replace(df, which(is.na(df), arr.ind = TRUE), 0)
    colnames(df) <- c('k', 'i', 'j')
    btn$obs <- df[1:nobs,]
    btn$nprobs <- nprobs
    btn_lines <- btn_lines[-c(1:nobs)]
  }
  
  # Data set 20
  data_set_20 <- rmti_parse_variables(btn_lines, n = 2, width = 10, character = TRUE)
  btn$chkmas <- as.logical(data_set_20$variables[1])
  btn$nprmas <- as.numeric(data_set_20$variables[2])
  btn_lines <- data_set_20$remaining_lines
  rm(data_set_20)
  
  btn$perlen <- NULL
  btn$nstp <- NULL
  btn$tsmult <- NULL
  btn$ssflag <- NULL
  btn$tslngh <- list()
  btn$dt0 <- NULL
  btn$mxstrn <- NULL
  btn$ttsmult <- NULL
  btn$ttsmax <- NULL
  
  for(i in 1:btn$nper) {  
    # Data set 21
    data_set_21 <- rmti_parse_variables(btn_lines, n = 4, width = 10, character = TRUE)
    btn$perlen[i] <- as.numeric(data_set_21$variables[1])
    btn$nstp[i] <- as.numeric(data_set_21$variables[2])
    btn$tsmult[i] <- as.numeric(data_set_21$variables[3])
    variables <- rmti_parse_variables(btn_lines, n = 0, format = 'free', character = TRUE)$variables
    btn$ssflag[i] <- 'SSTATE' %in% toupper(variables)
    btn_lines <- data_set_21$remaining_lines
    rm(data_set_21, variables)
    
    # Data set 22
    if(btn$tsmult[i] <= 0) {
      ntmp <- 0
      tslngh <- NULL
      while(ntmp < btn$nstp[i]) {
        n <- ifelse(btn$nstp[i] - ntmp > 8, 8, btn$nstp[i] - ntmp)
        data_set_22 <- rmti_parse_variables(btn_lines, n = n, width = 10)
        tslngh <- append(tslngh, as.numeric(data_set_22$variables[1:n]))
        btn_lines <- data_set_22$remaining_lines
        ntmp <- ntmp + n
      }
      btn$tslngh[[i]] <- tslngh
      rm(data_set_22)
    }
    
    # Data set 23
    data_set_23 <- rmti_parse_variables(btn_lines, n = 4, width = 10)
    btn$dt0[i] <- as.numeric(data_set_23$variables[1])
    btn$mxstrn[i] <- as.numeric(data_set_23$variables[2])
    btn$ttsmult[i] <- as.numeric(data_set_23$variables[3])
    btn$ttsmax[i] <- as.numeric(data_set_23$variables[4])
    btn_lines <- data_set_23$remaining_lines
    rm(data_set_23)
  }
  if(length(btn$tslngh) == 0) btn$tslngh <- NULL
  
  class(btn) <- c('btn','rmt_package')
  return(btn)
}

#' Write a MT3DMS basic transport package file
#' 
#' @param btn an \code{RMT3DMS} btn object
#' @param file filename to write to; typically '*.btn'
#' @param iprn format code for printing arrays in the listing file; defaults to -1 (no printing)
#' @param ... arguments passed to \code{rmti_write_array}. 
#' @return \code{NULL}
#' @export
#' @seealso \code{\link{rmt_read_btn}}, \code{\link{rmt_create_btn}}
rmt_write_btn <- function(btn,
                      file = {cat('Please select btn file to overwrite or provide new filename ...\n'); file.choose()},
                      iprn=-1,
                      ...) {
  
  mf_style <- btn$modflowstylearrays
  
  # Data set 1-2
  v <- packageDescription("RMT3DMS")$Version
  cat(paste('# MT3DMS Basic Transport File created by RMT3DMS, version',v,'\n'), file=file)
  cat(paste('#', comment(btn)), '\n', file=file, append=TRUE)
  
  # options
  if(any(btn$modflowstylearrays, btn$drycell, btn$legacy99storage, btn$ftlprint, btn$nowetdryprint, btn$omitdrycellbudget, btn$altwtsorb)) {
    rmti_write_variables(ifelse(btn$modflowstylearrays, 'MODFLOWSTYLEARRAYS', ''), ifelse(btn$drycell, 'DRYCELL', ''), ifelse(btn$legacy99storage, 'LEGACY99STORAGE', ''),
                         ifelse(btn$ftlprint, 'FTLPRINT', ''), ifelse(btn$nowetdryprint, 'NOWETDRYPRINT', ''), ifelse(btn$omitdrycellbudget, 'OMITDRYCELLBUDGET', ''),
                         ifelse(btn$altwtsorb, 'ALTWTSORB', ''), file = file, format = 'free')
  }
  
  # Data set 3
  rmti_write_variables(btn$nlay, btn$nrow, btn$ncol, btn$nper, btn$ncomp, btn$mcomp, file = file, integer = TRUE)

  # Data set 4
  rmti_write_variables(btn$tunit, btn$lunit, btn$munit, file = file, width = 4)
  
  # Data set 5
  rmti_write_variables(as.character(factor(btn$trnop,levels=c(TRUE,FALSE),labels=c(' T',' F'))), file = file, width = 2)
  
  # Data set 6
  nLines <- (btn$nlay %/% 40 + ifelse((btn$nlay %% 40) == 0, 0, 1))
  for(i in 1:nLines) {
    rmti_write_variables(btn$laycon[((i-1)*40+1) : ifelse((i*40) > btn$nlay, btn$nlay, (i*40))], file = file, width = 2, integer = TRUE)
  }
  
  # Data set 7
  rmti_write_array(btn$delr, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 8
  rmti_write_array(btn$delc, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 9
  rmti_write_array(btn$htop, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 10
  rmti_write_array(btn$dz, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 11
  rmti_write_array(btn$prsity, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 12
  rmti_write_array(btn$icbund, file = file, iprn = iprn, mf_style = mf_style, ...)
  
  # Data set 13
  for(species in 1:btn$ncomp) {
    btn$sconc[[species]][which(is.na(btn$sconc[[species]]))] <- btn$cinact
    rmti_write_array(btn$sconc[[species]], file = file, iprn = iprn, mf_style = mf_style, ...)
  }
  
  # Data set 14
  rmti_write_variables(btn$cinact, btn$thkmin, file = file)
  
  # Data set 15
  rmti_write_variables(as.integer(btn$ifmtcn), as.integer(btn$ifmtnp), as.integer(btn$ifmtrf), as.integer(btn$ifmtdp), as.character(factor(btn$savucn,levels=c(TRUE,FALSE),labels=c(' T',' F'))), file = file)
  
  # Data set 16
  rmti_write_variables(btn$nprs, file = file, integer = TRUE)
  
  # Data set 17
  if(btn$nprs > 0) {
    nLines <- (btn$nprs %/% 8 + ifelse((btn$nprs %% 8) == 0, 0, 1))
    for(i in 1:nLines) {
      rmti_write_variables(btn$timprs[((i-1)*8+1) : ifelse((i*8) > btn$nprs, btn$nprs, (i*8))], file = file)
    }
  }
  
  # Data set 18
  rmti_write_variables(ifelse(is.null(btn$obs), 0, nrow(btn$obs)), ifelse(is.null(btn$obs), 1, btn$nprobs), file = file, integer = TRUE)
  
  # Data set 19
  if(!is.null(btn$obs)) {
    # readr (fast)
    fmt <- paste0(rep('%10i', 3), collapse = '')
    dff <- do.call('sprintf', c(btn$obs[,c('k','i','j')], fmt))
    readr::write_lines(dff, file = file, append = TRUE)
  }
  
  # Data set 20
  rmti_write_variables(as.character(factor(btn$chkmas,levels = c(TRUE,FALSE),labels=c('T','F'))), as.integer(btn$nprmas), file = file)
  
  for(i in 1:btn$nper) {  
    # Data set 21
    rmti_write_variables(btn$perlen[i], as.integer(btn$nstp[i]), btn$tsmult[i], ifelse(btn$ssflag[i], 'SSTATE', ''), file = file)
    
    # Data set 22
    if(btn$tsmult[i] <= 0) {
      nLines <- (btn$nstp %/% 8 + ifelse((btn$nstp %% 8) == 0, 0, 1))
      for(j in 1:nLines) {
        rmti_write_variables(btn$tslngh[((j-1)*8+1) : ifelse((j*8) > btn$nstp, btn$nstp, (j*8))], file = file)
      }
    }
    
    # Data set 23
    rmti_write_variables(btn$dt0[i], as.integer(btn$mxstrn[i]), btn$ttsmult[i], btn$ttsmax[i], file = file)
  }
}
