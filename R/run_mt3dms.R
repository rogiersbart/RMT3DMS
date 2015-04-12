#' Run an MT3DMS model
#' 
#' \code{read.btn} reads in an MT3DMS basic transport package file and returns it as an \code{\link{RMT3DMS}} btn object.
#' 
#' @param file Filename; typically *.btn
#' @return Object of class btn
#' @export
run_mt3dms <- function(nam,dir=getwd(),mt3dmsVersion='mt3dms5b')
{
  shell(paste('cd',dir,'&',mt3dmsVersion,nam)) 
}