
#' Convert RMT3DMS btn to RMODFLOW dis object
#' 
#' @param btn btn object
#' @return dis object
#' @export
rmt_convert_btn_to_dis <- function(btn) {

  botm <- btn$dz*NA
  botm[,,1] <- btn$htop - btn$dz[,,1]
  if(btn$nlay >1) for(k in 2:btn$nlay) botm[,,k] <- botm[,,k-1]-btn$dz[,,k]
  
  dis <- RMODFLOW::rmf_create_dis(nlay = btn$nlay,
                                  nrow = btn$nrow,
                                  ncol = btn$ncol,
                                  nper = btn$nper,
                                  itmuni = btn$tunit,
                                  lenuni = btn$lunit,
                                  laycbd = rep(0, btn$nlay),
                                  delr = btn$delr,
                                  delc = btn$delc,
                                  top = btn$htop,
                                  botm = botm,
                                  perlen = btn$perlen,
                                  nstp = btn$nstp,
                                  tsmult = btn$tsmult)
  dis$sstr <- NA
  return(dis)
}

#' @describeIn rmt_convert_btn_to_dis Deprecated function name
#' @export
convert_btn_to_dis <- function(...) {
  .Deprecated(new = "rmt_convert_btn_to_dis", old = "convert_btn_to_dis")
  rmt_convert_btn_to_dis(...)
}


#' Convert a RMODFLOW object to a RMT3DMS object
#'
#' \code{rmf_convert_rmf_to_rmt} converts the class of a RMODFLOW object to the corresponding RMT3DMS class
#'
#' @param obj either a \code{rmf_2d_array}, \code{rmf_3d_array}, \code{rmf_4d_array} or a \code{rmf_list}
#'
#' @return obj with class \code{rmt_2d_array}, \code{rmt_3d_array}, \code{rmt_4d_array} or \code{rmt_list}
#' @export
#'
rmt_convert_rmf_to_rmt <- function(obj) {
  
  if(inherits(obj, 'rmf_2d_array')) {
    class(obj) <- replace(class(obj), which(class(obj) == 'rmf_2d_array'), 'rmt_2d_array')
  }
  if(inherits(obj, 'rmf_3d_array')) {
    class(obj) <- replace(class(obj), which(class(obj) == 'rmf_3d_array'), 'rmt_3d_array')
  }
  if(inherits(obj, 'rmf_4d_array')) {
    class(obj) <- replace(class(obj), which(class(obj) == 'rmf_4d_array'), 'rmt_4d_array')
  }
  if(inherits(obj, 'rmf_list')) {
    class(obj) <- replace(class(obj), which(class(obj) == 'rmf_list'), 'rmt_list')
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

#' @describeIn rmt_create_array Deprecated function name
#' @export
create_rmt3dms_array <- function(...) {
  .Deprecated(new = "rmt_create_array", old = "create_rmt3dms_array")
  rmt_create_array(...)
}

#' @export
"[.rmt_4d_array" <-  function(x, i, j, k, l, ...) {
  if(missing(i) && missing(j) && missing(k) && missing(l)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1, missing(l) || length(l) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  # l missing -> always 4d unless all other indices are given
  if(!drop && sum(miss) > 1) {
    if(!miss[4]) {
      dim(obj) <- dim(obj)[miss]
    } 
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmt_4d_array', 'rmt_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- replace(class(x), class(x) == 'rmt_4d_array', 'rmt_3d_array')
  } 
  else if (length(dim(obj)) == 4) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmt_4d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[rmfi_ifelse0(miss[4], rmfi_ifelse0(!drop && sum(miss) > 1, rep(TRUE, 4), miss), miss)]
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
  if(missing(i) && missing(j) && missing(k)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1, missing(k) || length(k) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1) {
    dim(obj) <- dim(obj)[miss]
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- replace(class(x), class(x) == 'rmt_3d_array', 'rmt_2d_array')
  }
  else if (length(dim(obj)) == 3) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmt_3d_array')
  }
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
  if(is.null(dim(obj))) attributes(obj) <- NULL
  
  return(obj)
}

#' @export
"[.rmt_2d_array" <-  function(x, i, j, ...) {
  if(missing(i) && missing(j)) return(x)
  miss <- c(missing(i) || length(i) > 1, missing(j) || length(j) > 1)
  drop <- ifelse('drop' %in% names(list(...)), list(...)[['drop']], sum(miss) < 2)
  
  obj <-  NextMethod(..., drop = drop)
  
  if(!drop && sum(miss) > 1) {
    dim(obj) <- dim(obj)[miss]
  } 
  
  if (length(dim(obj)) == 2) {
    class(obj) <- class(x)
  } else {
    class(obj) <- subset(class(x), class(x) != 'rmt_2d_array')
  }
  
  attrs <- attributes(obj)
  id <- names(attributes(x))
  id <- id[!(id %in% c('dim', 'class'))]
  if(length(id) > 0) attributes(obj) <- append(attrs, attributes(x)[id])
  attr(obj, 'dimlabels') <- attr(obj, 'dimlabels')[miss]
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

