#' Read an MT3DMS mass balance summary file
#' 
#' \code{read_mas} reads in an MT3DMS mass balance summary file and returns it as an \code{\link{RMT3DMS}} mas object.
#' 
#' @param file filename; typically '*.mas'
#' @return object of class mas
#' @export
read_mas <- function(file = {cat('Please select mas file...\n'); file.choose()}) {
  
  mas <- read.table(file,skip=2)
  names(mas) <- c('TIME','TOTal_IN','TOTal_OUT','SOURCES','SINKS','NET_MASS_FROM_FLUID_STORAGE','TOTal_MASS_IN_AQUIFER','DISCREPANCY_TOTal_IN_OUT','DISCREPANCY_alTERNATIVE')
  class(mas) <- c('mas','mt3dms_package')
  return(mas)
}
