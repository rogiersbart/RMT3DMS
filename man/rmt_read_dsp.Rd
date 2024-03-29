% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-dsp.R
\name{rmt_read_dsp}
\alias{rmt_read_dsp}
\title{Read an MT3DMS dispersion package file}
\usage{
rmt_read_dsp(
  file = {     cat("Please select dsp file ...\\n")     file.choose() },
  btn = {     cat("Please select corresponding btn file ...\\n")    
    rmt_read_btn(file.choose()) },
  ...
)
}
\arguments{
\item{file}{filename; typically '*.dsp'}

\item{btn}{\code{RMT3DMS} btn object}

\item{...}{optional arguments passed to \code{\link{rmti_parse_array}}}
}
\value{
object of class dsp
}
\description{
\code{rmt_read_dsp} reads in an MT3DMS dispersion package file and returns it as an \code{\link{RMT3DMS}} dsp object.
}
\seealso{
\code{\link{rmt_create_dsp}}, \code{\link{rmt_write_dsp}}
}
