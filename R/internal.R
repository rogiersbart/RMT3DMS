#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
read_mt3dms_array <- function(remaining_lines, nrow, ncol, nlay, ndim = NULL) {
  data_set <- RMODFLOW:::read_modflow_array(remaining_lines, nrow, ncol, nlay, ndim)
  class(data_set$array) <- gsub('modflow','mt3dms',class(data_set$array))
  return(data_set)
}


#' Read mt3dms variables
#' If all are numbers, returns numeric, otherwise returns character vector
read_mt3dms_variables <- function(remaining_lines) {
  return(RMODFLOW:::read_modflow_variables(remaining_lines))
}

#' Remove empty elements from a vector of strings.
#' @param vector_of_strings Vector of strings.
#' @return Vector of strings without the empty items.
remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}
