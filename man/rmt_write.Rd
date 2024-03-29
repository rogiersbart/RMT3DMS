% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mt3dms.R
\name{rmt_write}
\alias{rmt_write}
\title{Write a MT3DMS model}
\usage{
rmt_write(
  mt3dms,
  file = {    
    cat("Please select nam file to overwrite or provide new filename ...\\n")    
    file.choose() },
  exclude = NULL,
  suppress = FALSE,
  verbose = TRUE,
  iprn = -1
)
}
\arguments{
\item{mt3dms}{\code{RMT3DMS} mt3dms object}

\item{file}{filename of the name file to write}

\item{exclude}{character vector with packages names to exclude from the simulation. Defaults to NULL}

\item{suppress}{logical; remove non-supported (and thus not written) packages in the NAME file ? Defaults to FALSE}

\item{verbose}{logical; should information on writing files be printed to the console ? Defaults to TRUE.}

\item{iprn}{format code for printing arrays in the listing file; defaults to -1 (no printing)}
}
\value{
\code{NULL}
}
\description{
\code{rmt_write} writes all input packages in a \code{RMT3DMS} mt3dms object to a directory
}
\details{
All arrays use IREAD 0 or 103 (constant/free) unless \code{mt3dms$btn$modflowstylearrays = TRUE} in which case free-format headers INTERNAL or CONSTANT are used.
         All packages will be written according to the filenames (fname) defined in the nam object.
         To prevent any files being overwritten, it is best to write to an empty directory.
}
\examples{
btn <- rmt_create_btn()
adv <- rmt_create_adv()
gcg <- rmt_create_gcg()
nam <- rmt_create_nam(btn, adv, gcg, ftl = 'output.ftl', ftl_free = TRUE)

m <- rmt_create(btn, adv, gcg, nam)
f <- tempfile()

rmt_write(m, f, verbose = FALSE)

f2 <- tempfile()
\dontrun{
rmt_write(m, f2, exclude = 'dsp')
}
}
\seealso{
\code{\link{rmt_create}}, \code{\link{rmt_read}}
}
