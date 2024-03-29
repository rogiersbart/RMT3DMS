
#' Create a rmt_list object
#'
#' @param df a data.frame like object with at least columns \code{i, j, k, itype} and one or more \code{css} columns specifying the concentrations
#' @param kper integer value(s) specifying during which stress-periods this list is active
#'
#' @return a \code{rmt_list} object
#' @export
#'
#' @examples
#' df <- data.frame(i = 5, j = 4:5, k = 3, css1 = 6.45, css2 = 47, itype = 2)
#' rmt_create_list(df, kper = c(1, 3))
rmt_create_list <- function(df, kper) {
  if(missing(kper)) stop('Please supply kper argument', call. = FALSE)
  df <- RMODFLOW::rmf_create_list(df, kper = kper)
  if(!('itype' %in% colnames(df)) || !('css' %in% colnames(df) || 'css1' %in% colnames(df))) stop('df object should at least have columns k, i, j, css, itype', call. = FALSE)
  if('css' %in% colnames(df)) colnames(df) <- replace(colnames(df), which(colnames(df) == 'css'), 'css1')
  class(df) <- c('rmt_list', class(df))
  return(df)
}

#' Generic to convert objects to rmt_list
#'
#' @export
#' @rdname rmt_as_list
rmt_as_list <- function(...) {
  UseMethod('rmt_as_list')
}


#' @param wel \code{RMODFLOW} wel object
#' @param cwel concentrations in the wells for species 1. A value must be supplied for every well. Alternatively, a character can be entered referencing a column in the wel data set to use.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @param ... additional numeric vectors containing concentrations (one value for each feature) or column names. One vector/name for each species > 1 (in order).
#'
#' @details kper should be one of the stress periods defined in the flow model unless only one stress period is present in the flow model.
#' @return a \code{rmt_list} object
#' @export
#' @rdname rmt_as_list
#' @method rmt_as_list wel
#' @examples
#' library(RMODFLOW)
#' m <- rmf_read(rmf_example_file('example-model.nam'), verbose = FALSE)
#' rmt_as_list(m$wel, 6.5, kper = 1)
#' 
#' m$wel$data$chloride <- 6.5
#' rmt_as_list(m$wel, 'chloride', kper = 1)
rmt_as_list.wel <- function(wel, cwel, kper, ...) {
  itype <- 2
  arg <- list(...)
  arg <- c(cwel, arg)
  arg <- lapply(arg, function(i) if(length(i) == 1 && is.character(i)) wel$data[[i]] else i)
  conc <- do.call(cbind, arg)

  df <- rmti_create_bc_list(wel, conc, itype = itype, kper = kper)
  return(df)
}

#' @param drn \code{RMODFLOW} drn object
#' @param cdrn concentrations in the drains for species 1. A value must be supplied for every drain. Alternatively, a character can be entered referencing a column in the drn data set to use.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @param ... additional numeric vectors containing concentrations (one value for each feature) or column names. One vector/name for each species > 1 (in order).
#'
#' @return a \code{rmt_list} object
#' @export
#' @rdname rmt_as_list
#' @method rmt_as_list drn
rmt_as_list.drn <- function(drn, cdrn, kper, ...) {
  itype <- 3
  arg <- list(...)
  arg <- c(cdrn, arg)
  arg <- lapply(arg, function(i) if(length(i) == 1 && is.character(i)) drn$data[[i]] else i)
  conc <- do.call(cbind, arg)
  
  df <- rmti_create_bc_list(drn, conc, itype = itype, kper = kper)
  return(df)
}

#' @param chd \code{RMODFLOW} chd object
#' @param cchd concentrations in the constant-head cells for species 1. A value must be supplied for every constant-head cell. Alternatively, a character can be entered referencing a column in the chd data set to use.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @param ... additional numeric vectors containing concentrations (one value for each feature) or column names. One vector/name for each species > 1 (in order).
#'
#' @return a \code{rmt_list} object
#' @export
#' @rdname rmt_as_list
#' @method rmt_as_list chd
#' @examples
#' # 3 species
#' cchd1 <- rnorm(nrow(m$chd$data), 70, 50)
#' rmt_as_list(m$chd, cchd1, kper = c(2, 3), cchd1 * 2, cchd1 / 2)
#' 
#' # stress periods should be the same when flow object has more than 1 stress period
# m$chd$kper <- m$chd$kper[c(1,1),]
# m$chd$kper$kper <- c(1,2)
#' \dontrun{
#' rmt_as_list(m$chd, cchd1, kper = c(2, 3))
#' }
rmt_as_list.chd <- function(chd, cchd, kper, ...) {
  itype <- 1
  arg <- list(...)
  arg <- c(list(cchd), arg)
  arg <- lapply(arg, function(i) if(length(i) == 1 && is.character(i)) chd$data[[i]] else i)
  conc <- do.call(cbind, arg)
  
  df <- rmti_create_bc_list(chd, conc, itype = itype, kper = kper)
  return(df)
}

#' @param riv \code{RMODFLOW} riv object
#' @param criv concentrations in the river cells for species 1. A value must be supplied for every river cell. Alternatively, a character can be entered referencing a column in the riv data set to use.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @param ... additional numeric vectors containing concentrations (one value for each feature) or column names. One vector/name for each species > 1 (in order).
#'
#' @return a \code{rmt_list} object
#' @export
#' @rdname rmt_as_list
#' @method rmt_as_list riv
rmt_as_list.riv <- function(riv, criv, kper, ...) {
  itype <- 4
  arg <- list(...)
  arg <- c(criv, arg)
  arg <- lapply(arg, function(i) if(length(i) == 1 && is.character(i)) riv$data[[i]] else i)
  conc <- do.call(cbind, arg)
  
  df <- rmti_create_bc_list(riv, conc, itype = itype, kper = kper)
  return(df)
}

#' @param ghb \code{RMODFLOW} ghb object
#' @param cghb concentrations in the general-head boundary cells for species 1. A value must be supplied for every general-head boundary cell. Alternatively, a character can be entered referencing a column in the ghb data set to use.
#' @param kper numeric vector with the stress period numbers during which the list is active.
#' @param ... additional numeric vectors containing concentrations (one value for each feature) or column names. One vector/name for each species > 1 (in order).
#'
#' @return a \code{rmt_list} object
#' @export
#' @rdname rmt_as_list
#' @method rmt_as_list ghb
rmt_as_list.ghb <- function(ghb, cghb, kper, ...) {
  itype <- 5
  arg <- list(...)
  arg <- c(cghb, arg)
  arg <- lapply(arg, function(i) if(length(i) == 1 && is.character(i)) ghb$data[[i]] else i)
  conc <- do.call(cbind, arg)
  
  df <- rmti_create_bc_list(ghb, conc, itype = itype, kper = kper)
  return(df)
}

#' @export
as.data.frame.rmt_list <- function(obj, ...) structure(NextMethod(...), kper = NULL, solute = NULL)

#' Convert RMT3DMS dis to RMT3DMS btn object
#'
#' @param dis \code{RMODFLOW} dis object
#' @param nper number of stress periods in the btn object. Defaults to \code{dis$nper}. See details.
#' @param perlen vector of stress period lengths in the btn object. Defaults to \code{dis$perlen[1:nper]}. See details.
#' @param tunit character specifying the time unit. The default (NULL), guesses from the dis object. Note that tunit is for identification purposes only does not affect the model outcome.
#' @param tunit character specifying the length unit. The default (NULL), guesses from the dis object. Note that lunit is for identification purposes only does not affect the model outcome.
#' @param prj \code{RMODFLOW} prj object. By default, obtained from the dis object.
#' @param ... additional arguments passed to \code{rmt_create_btn}
#'
#' @return \code{RMT3DMS} btn object
#' @details Conversion works best when number of stress-periods is equal in the dis and btn objects.
#'  \code{nper}, \code{perlen}, \code{nstp} & \code{tsmult} should only differ from the defaults when the flow model is steady-state 
#'  with only 1 stress period. In that case, MT3DMS allows for differing stress periods between the flow and transport model.
#' @export
#' @seealso \code{\link{rmt_convert_btn_to_dis}}, \code{\link{rmt_create_btn}}
#' @examples
#' library(RMODFLOW)
#' dis <- rmf_read_dis(rmf_example_file('example-model.dis'))
#' rmt_convert_dis_to_btn(dis)
#' rmt_convert_dis_to_btn(dis, nper = 2, perlen = c(1000, 500), tunit = 'd', ncomp = 2)
#' 
rmt_convert_dis_to_btn <- function(dis, nper = dis$nper, perlen = dis$perlen[1:nper],
                                   tunit = NULL, lunit = NULL, prj = RMODFLOW::rmf_get_prj(dis), ...) {
  
  dz <- rmt_convert_rmf_to_rmt(RMODFLOW::rmf_calculate_thickness(dis))
  
  tunit <- ifelse(is.null(tunit), switch(dis$itmuni + 1, 'undf', 's', 'min', 'h', 'd', 'y'), tunit)
  lunit <- ifelse(is.null(lunit), switch(dis$lenuni + 1, 'undf', 'ft', 'm', 'cm'), lunit)
  
  btn <- rmt_create_btn(nlay = dis$nlay,
                        nrow = dis$nrow,
                        ncol = dis$ncol,
                        nper = nper,
                        tunit = tunit,
                        lunit = lunit,
                        delr = dis$delr,
                        delc = dis$delc, 
                        htop = dis$top,
                        dz = dz,
                        perlen = perlen,
                        nstp = ifelse(dis$sstr == 'SS', 1, dis$nstp),
                        tsmult = dis$tsmult,
                        prj = prj,
                        ...)
  return(btn)
}

#' Convert RMT3DMS btn to RMODFLOW dis object
#' 
#' @param btn \code{RMT3DMS} btn object
#' @param nper number of stress periods in the dis object. Defaults to \code{btn$nper}. See details.
#' @param perlen vector of stress period lengths in the dis object. Defaults to \code{btn$perlen[1:nper]}. See details.
#' @param nstp vector of stress period time steps in the dis object. Defaults to \code{btn$nstp[1:nper]}. See details.
#' @param tsmult vector of stress period time step multipliers in the dis object. Defaults to \code{btn$tsmult[1:nper]}. See details.
#' @param sstr character vector with steady state ('SS') or transient ('TR') stress period indicator; defaults to first period 'SS' and all others 'TR'
#' @param prj \code{RMODFLOW} prj object. By default obtained from the \code{btn} object.
#' @param ... additional arguments passed to \code{RMODFLOW::rmf_create_dis}
#'
#' @return \code{RMODFLOW} dis object
#' @details Conversion works best when number of stress-periods is equal in the dis and btn objects.
#'  \code{nper}, \code{perlen}, \code{nstp} & \code{tsmult} should only differ from the defaults when the flow model is steady-state 
#'  with only 1 stress period. In that case, MT3DMS allows for differing stress periods between the flow and transport model.
#' @export
#' @seealso \code{\link{rmt_convert_dis_to_btn}}
#' @examples 
#' btn <- rmt_create_btn()
#' rmt_convert_btn_to_dis(btn)
#' 
#' # BTN with 2 stress periods but flow model is single stress period steady-state
#' btn <- rmt_create_btn(nper = 2, perlen = c(1000, 500))
#' rmt_convert_btn_to_dis(btn, nper = 1, perlen = 1, nstp = 1, tsmult = 1)
#' 
#' # otherwise converts to dis object with only first stress period steady-state
#' rmt_convert_btn_to_dis(btn)
#' 
rmt_convert_btn_to_dis <- function(btn,
                                   nper = btn$nper, 
                                   perlen = btn$perlen[1:nper],
                                   nstp = btn$nstp[1:nper],
                                   tsmult = btn$tsmult[1:nper],
                                   sstr = c("SS", rep("TR", nper - 1)), 
                                   prj = RMODFLOW::rmf_get_prj(btn),
                                   ...) {

  botm <- btn$dz*NA
  botm[,,1] <- btn$htop - btn$dz[,,1]
  if(btn$nlay >1) for(k in 2:btn$nlay) botm[,,k] <- botm[,,k-1]-btn$dz[,,k]
  itmuni <- toupper(btn$tunit)
  lenuni <- toupper(btn$lunit)
  
  if(itmuni %in% c('S', 'SEC', 'SECON', 'SECOND', 'SECONDS')) {
    itmuni <- 1
  } else if(itmuni %in% c('M', 'MIN', 'MINUT', 'MINUTE', 'MINUTES')) {
    itmuni <- 2
  } else if(itmuni %in% c('H', 'HOUR', 'HOURS')) {
    itmuni <- 3
  } else if(itmuni %in% c('D', 'DAY', 'DAYS')) {
    itmuni <- 4
  } else if(itmuni %in% c('Y', 'YEAR', 'YEARS')) {
    itmuni <- 5
  } else {
    itmuni <- 0
  }
  
  if(lenuni %in% c('FT', 'FEET', 'FOOT')) {
    lenuni <- 1
  } else if(lenuni %in% c('M', 'METE', 'METER', 'METERS', 'METRE', 'METR', 'METRES')) {
    lenuni <- 2
  } else if(lenuni %in% c('CM', 'CENT', 'CNTM', 'CNT', 'CENTIMETER', 'CENTIMETERS', 'CENTIMETRE', 'CENTIMETRES')) {
    lenuni <- 3
  } else {
    lenuni <- 0
  }
  
  
  
  dis <- RMODFLOW::rmf_create_dis(nlay = btn$nlay,
                                  nrow = btn$nrow,
                                  ncol = btn$ncol,
                                  nper = nper,
                                  itmuni = itmuni,
                                  lenuni = lenuni,
                                  laycbd = rep(0, btn$nlay),
                                  delr = btn$delr,
                                  delc = btn$delc,
                                  top = btn$htop,
                                  botm = botm,
                                  perlen = perlen,
                                  nstp = nstp,
                                  tsmult = tsmult,
                                  sstr = sstr,
                                  prj = prj,
                                  ...)
  return(dis)
}

#' Convert a tob object to a locations data frame
#'
#' \code{rmt_convert_tob_to_locations} converts a tob object holding concentration observations to a locations data frame 
#'
#' @param hob \code{RMT3DMS} tob object
#' @param btn \code{RMT3DMS} btn object
#' @param prj prj object
#' @details returned z coordinate represents the center of the cell, not the top or bottom of the well screen. 
#'          The returned locations represent the concentration observations, not the mass flux observations.
#' @return a data frame containing name, screened layer proportion pr, cell indices k, i, j and x, y & z coordinates
#' @export
#' @seealso \code{\link{rmt_create_tob}}
rmt_convert_tob_to_locations <- function(tob,
                                         btn,
                                         prj = RMODFLOW::rmf_get_prj(btn)) {
  
  if(tob$inconcobs == 0) stop('tob object does not contain concentration observations', call. = FALSE)
  tob$concentrations <- tob$concentrations[!duplicated(tob$concentrations$cobsnam),]
  if(any(lengths(tob$concentrations$layer) > 1)) {
    m_id <- which(lengths(tob$concentrations$layer) > 1)
    df <- tob$concentrations[-m_id, ]
    df$layer <- unlist(df$layer)
    df$pr <- unlist(df$pr)
    df_m <- lapply(m_id, function(i) as.data.frame(lapply(tob$concentrations[i, ], unlist)))
    df_m <- do.call(rbind, df_m)
    df <- rbind(df, df_m) 
  } else {
    df <- tob$concentrations
    df$layer <- unlist(df$layer)
    df$pr <- unlist(df$pr)
  }
  
  dis <- rmt_convert_btn_to_dis(btn, prj = prj)
  # TODO top & bottom filter
  coords <- RMODFLOW::rmf_convert_grid_to_xyz(i = df$row, j = df$column, k = df$layer, roff = df$roff, coff=df$coff, dis = dis, prj = prj)
  locations <- cbind(subset(df, select = c('cobsnam', 'pr', 'layer', 'row', 'column')), coords)
  locations <- setNames(locations, c('name', 'pr', 'k', 'i', 'j', 'x', 'y', 'z'))
  return(locations)
}

#' Convert a RMODFLOW object to a RMT3DMS object
#'
#' \code{rmf_convert_rmf_to_rmt} adds the corresponding RMT3DMS class to an RMODFLOW object 
#'
#' @param obj either a \code{rmf_2d_array}, \code{rmf_3d_array}, \code{rmf_4d_array} or a \code{rmf_list}
#'
#' @return obj with class \code{rmt_2d_array}, \code{rmt_3d_array}, \code{rmt_4d_array} or \code{rmt_list}
#' @export
#'
rmt_convert_rmf_to_rmt <- function(obj) {
  
  if(inherits(obj, 'rmf_2d_array')) {
    class(obj) <- c('rmt_2d_array', class(obj))
  }
  if(inherits(obj, 'rmf_3d_array')) {
    class(obj) <- c('rmt_3d_array', class(obj))
  }
  if(inherits(obj, 'rmf_4d_array')) {
    class(obj) <- c('rmt_4d_array', class(obj))
  }
  if(inherits(obj, 'rmf_list')) {
    class(obj) <- c('rmt_list', class(obj))
  }
  
  return(obj)
}

#' Add rmt3dms array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @param solute integer vector specifying the solute species this array represents. Defaults to \code{NULL}
#' @param kper integer vector specifying the stress periods in which the array is active. Used for defining boundary conditions. Defaults to \code{NULL}
#' @param dimlabels character vector specifying the labels of the dimensions; defaults to \code{i, j, k, l} for the first, second, third and fourth dimension, respectively.
#' @details subsetting a \code{rmt_array} will return a \code{rmt_array} as long as the object has a dim argument (i.e. has 2 or more free dimensions). Atomic vectors are therefore never \code{rmt_arrays}. 
#'          When \code{l} is not specified when subsetting a \code{rmt_4d_array}, a \code{rmt_4d_array} will always be returned.
#'          Furthermore, unlike subsetting \code{arrays}, dimensions with length 1 will not be dropped unless the \code{drop} argument is set to \code{TRUE}
#' @return either a \code{rmt_2d_array}, a \code{rmt_3d_array} or \code{rmt_4d_array} object
#' @export
rmt_create_array <- function(obj = NA, 
                             dim = NULL, 
                             solute = attr(obj, 'solute'),
                             kper = attr(obj, 'kper'),
                             dimlabels = attr(obj, 'dimlabels')) {
  array <- RMODFLOW::rmf_create_array(obj = obj, dim = dim, kper = kper, dimlabels = dimlabels)
  array <- rmt_convert_rmf_to_rmt(array)
  attr(array, 'solute') <- solute
  return(array)
}

#' @export
"[.rmt_4d_array" <-  function(x, i, j, k, l, ...) {
  attr(x, 'dimnames') <- NULL
  if(missing(i) && missing(j) && missing(k) && missing(l)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1, missing(l) || length(l) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  drop_given <- 'drop' %in% names(list(...))
  
  obj <-  NextMethod(..., drop = drop)
  
  # l missing -> always 4d unless all other indices are given
  if(!drop && sum(miss) > 1 && !drop_given) {
    if(!miss[4]) {
      dim(obj) <- dim(obj)[miss]
      rst_lbl <- TRUE
    } else {
      rst_lbl <- FALSE
    }
  } else {
    rst_lbl <- FALSE
  }
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmt_4d_array', 'rmt_2d_array')
    class(obj) <- replace(class(obj), class(obj) == 'rmf_4d_array', 'rmf_2d_array')
    rst_lbl <- TRUE
  } else if (length(dim(obj)) == 3) {
    class(obj) <- replace(class(x), class(x) == 'rmt_4d_array', 'rmt_3d_array')
    class(obj) <- replace(class(obj), class(obj) == 'rmf_4d_array', 'rmf_3d_array')
    rst_lbl <- TRUE
  } else if (length(dim(obj)) == 4) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), !(class(x) %in% c('rmt_4d_array', 'rmf_4d_array')))
    rst_lbl <- TRUE
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  if(rst_lbl) attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[rmti_ifelse0(miss[4], rmti_ifelse0(!drop && sum(miss) > 1, rep(TRUE, 4), miss), miss)]
  if(!missing(l)) {
    if(!is.null(attr(obj,'kstp'))) attr(obj,'kstp') <- attr(obj,'kstp')[l]
    if(!is.null(attr(obj,'kper'))) attr(obj,'kper') <- attr(obj,'kper')[l]
    if(!is.null(attr(obj,'pertim'))) attr(obj,'pertim') <- attr(obj,'pertim')[l]
    if(!is.null(attr(obj,'totim'))) attr(obj,'totim') <- attr(obj,'totim')[l]
    if(!is.null(attr(obj,'nstp'))) attr(obj,'nstp') <- attr(obj,'nstp')[l]
  }
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
"[.rmt_3d_array" <-  function(x, i, j, k, ...) {
  attr(x, 'dimnames') <- NULL
  if(missing(i) && missing(j) && missing(k)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  drop_given <- 'drop' %in% names(list(...))
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1 && !drop_given) {
    dim(obj) <- dim(obj)[miss]
    rst_lbl <- TRUE
  } else {
    rst_lbl <- FALSE
  }
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmt_3d_array', 'rmt_2d_array')
    class(obj) <- replace(class(obj), class(obj) == 'rmf_3d_array', 'rmf_2d_array')
    rst_lbl <- TRUE
  } else if (length(dim(obj)) == 3) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), !(class(x) %in% c('rmt_3d_array', 'rmf_3d_array')))
    rst_lbl <- TRUE
  }
  
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  if(rst_lbl) attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
"[.rmt_2d_array" <-  function(x, i, j, ...) {
  attr(x, 'dimnames') <- NULL
  if(missing(i) && missing(j)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  drop_given <- 'drop' %in% names(list(...))
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1 && !drop_given) {
    dim(obj) <- dim(obj)[miss]
    rst_lbl <- TRUE
  } else {
    rst_lbl <- FALSE
  }
  
  if (length(dim(obj)) == 2) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), !(class(x) %in% c('rmt_2d_array', 'rmf_2d_array')))
    rst_lbl <- TRUE
  }
  
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  if(rst_lbl) attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
as.matrix.rmt_2d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmt_3d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.matrix.rmt_4d_array <- function(obj) as.matrix(as.array(obj))

#' @export
as.array.rmt_2d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, solute = NULL, kper = NULL)

#' @export
as.array.rmt_3d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, solute = NULL, kper = NULL)

#' @export
as.array.rmt_4d_array <- function(obj) structure(obj, dimlabels = NULL, class = NULL, solute = NULL, kper = NULL)

#' @export
aperm.rmt_2d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
aperm.rmt_3d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
aperm.rmt_4d_array <- function(a, perm, ...) {
  att <- attributes(a)
  a <- aperm.default(a, perm = perm, ...)
  att$dimlabels <- att$dimlabels[perm]
  att$dim <- attr(a, 'dim')
  attributes(a) <- att
  return(a)
}

#' @export
apply.rmt_2d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
apply.rmt_3d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
apply.rmt_4d_array <- function(X, ...) {
  apply(as.array(X), ...)
}

#' @export
t.rmt_2d_array <- function(obj) {
  obj <- t.default(obj)
  attr(obj, "dimlabels") <- rev(attr(obj, "dimlabels"))
  return(obj)
}
