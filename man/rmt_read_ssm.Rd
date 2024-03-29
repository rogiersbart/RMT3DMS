% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package-ssm.R
\name{rmt_read_ssm}
\alias{rmt_read_ssm}
\title{Read an MT3DMS sink & source mixing package file}
\usage{
rmt_read_ssm(
  file = {     cat("Please select ssm file ...\\n")     file.choose() },
  btn = {     cat("Please select corresponding btn file ...\\n")    
    rmt_read_btn(file.choose()) },
  ftl = {     cat("Please select corresponding ftl file ...\\n")     file.choose() },
  ftl_free = NULL,
  ...
)
}
\arguments{
\item{file}{filename; typically '*.ssm'}

\item{btn}{\code{RMT3DMS} btn object}

\item{ftl}{path the to flow-transport link file. See details.}

\item{ftl_free}{logical; is the flow-transport link file written in free (formatted) format (TRUE) or binary (unformatted) (FALSE)? if NULL (default), it is guessed from reading \code{ftl}}

\item{...}{additional arguments passed to \code{rmti_parse_array}}
}
\value{
object of class \code{ssm}
}
\description{
\code{rmt_read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \link{RMT3DMS} ssm object.
}
\details{
\code{ftl} must be supplied to ensure the correct flow terms are read.
}
\seealso{
\code{\link{rmt_create_ssm}}, \code{\link{rmt_write_ssm}}
}
