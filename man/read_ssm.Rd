% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ssm.R
\name{read_ssm}
\alias{read_ssm}
\title{Read an MT3DMS sink & source mixing package file}
\usage{
read_ssm(file = {     cat("Please select ssm file ...\\n")     file.choose()
  }, btn = read_btn())
}
\arguments{
\item{file}{filename; typically '*.ssm'}

\item{btn}{basic transport package file object}
}
\value{
object of class ssm
}
\description{
\code{read_ssm} reads in an MT3DMS sink & source mixing package file and returns it as an \code{\link{RMT3DMS}} ssm object.
}

