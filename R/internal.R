#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
read_mt3dms_array <- function(remaining_lines, nrow, ncol, nlay, ndim = NULL) {
  data_set <- RMODFLOW::read_modflow_array(remaining_lines, nrow, ncol, nlay, ndim)
  class(data_set) <- rmt3dms_class(data_set)
  return(data_set)
}

#' Update RMODFLOW class to RMT3DMS class
rmt3dms_class <- function(rmodflow_object) {
  gsub('modflow','mt3dms',class(rmodflow_object))
}
