% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-nam.R
\name{rmt_write_nam}
\alias{rmt_write_nam}
\title{Write a MT3DMS name file}
\usage{
rmt_write_nam(
  nam,
  file = {    
    cat("Please select nam file to overwrite or provide new filename ...\\n")    
    file.choose() },
  exclude = NULL
)
}
\arguments{
\item{nam}{an \code{\link{RMT3DMS}} nam object}

\item{file}{filename to write to; typically '*.mt_nam'}

\item{exclude}{character vector with packages names to exclude from the simulation. Defaults to NULL}
}
\value{
\code{NULL}
}
\description{
\code{rmt_write_nam} writes a MT3DMS name file based on an \code{\link{RMT3DMS}} nam object.
}
\examples{
btn <- rmt_create_btn()
adv <- rmt_create_adv()
dsp <- rmt_create_dsp(btn = btn)
gcg <- rmt_create_gcg()
nam <- rmt_create_nam(btn, adv, gcg, btn, dsp, ftl = 'output.ftl', ftl_free = FALSE)
f <- tempfile()
rmt_write_nam(nam, file = f, exclude = 'dsp')

}
\seealso{
\code{\link{rmt_read_nam}}, \code{\link{rmt_create_nam}}
}
