% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-tob.R
\name{rmt_write_tob}
\alias{rmt_write_tob}
\title{Write a MT3DMS transport observation package file}
\usage{
rmt_write_tob(
  tob,
  file = {    
    cat("Please select tob file to overwrite or provide new filename ...\\n")    
    file.choose() }
)
}
\arguments{
\item{tob}{an \code{RMT3DMS} tob object}

\item{file}{filename to write to; typically '*.tob'}
}
\value{
\code{NULL}
}
\description{
Write a MT3DMS transport observation package file
}
\seealso{
\code{\link{rmt_read_tob}}, \code{\link{rmt_create_tob}}
}
