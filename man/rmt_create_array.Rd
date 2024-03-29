% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rmt_create_array}
\alias{rmt_create_array}
\title{Add rmt3dms array class to object based on object dimensions}
\usage{
rmt_create_array(
  obj = NA,
  dim = NULL,
  solute = attr(obj, "solute"),
  kper = attr(obj, "kper"),
  dimlabels = attr(obj, "dimlabels")
)
}
\arguments{
\item{obj}{object to add class to}

\item{dim}{the dim attribute for the array to be created; by default, dim(obj) is used}

\item{solute}{integer vector specifying the solute species this array represents. Defaults to \code{NULL}}

\item{kper}{integer vector specifying the stress periods in which the array is active. Used for defining boundary conditions. Defaults to \code{NULL}}

\item{dimlabels}{character vector specifying the labels of the dimensions; defaults to \code{i, j, k, l} for the first, second, third and fourth dimension, respectively.}
}
\value{
either a \code{rmt_2d_array}, a \code{rmt_3d_array} or \code{rmt_4d_array} object
}
\description{
Add rmt3dms array class to object based on object dimensions
}
\details{
subsetting a \code{rmt_array} will return a \code{rmt_array} as long as the object has a dim argument (i.e. has 2 or more free dimensions). Atomic vectors are therefore never \code{rmt_arrays}. 
         When \code{l} is not specified when subsetting a \code{rmt_4d_array}, a \code{rmt_4d_array} will always be returned.
         Furthermore, unlike subsetting \code{arrays}, dimensions with length 1 will not be dropped unless the \code{drop} argument is set to \code{TRUE}
}
