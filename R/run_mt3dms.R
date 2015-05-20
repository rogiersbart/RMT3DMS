#' Run an MT3DMS model
#' 
#' \code{run_mt3dms} runs an MT3DMS model.
#' 
#' @param file Path to name file; typically "*.nam"
#' @export
run_mt3dms <- function(file,mt3dms_executable='mt3dms5b')
{
  dir <- dirname(file)
  file <- basename(file)
  shell(paste('cd',dir,'&',mt3dms_executable,file),mustWork=TRUE) 
}