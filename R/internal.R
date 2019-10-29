#' Get an array specified by a free-format control record from the text lines analyzed in an \code{\link{RMODFLOW}} \code{read.*} function
#' @param object MODFLOW input file text object, starting with the free-format control record
#' @return A list containing the array and the remaining text of the MODFLOW input file
rmti_parse_array <- function(remaining_lines, nrow, ncol, nlay, ndim = NULL) {
  data_set <- RMODFLOW:::rmfi_parse_array(remaining_lines, nrow, ncol, nlay, ndim)
  class(data_set$array) <- gsub('modflow','mt3dms',class(data_set$array))
  return(data_set)
}

#' @describeIn rmti_parse_array Deprecated function name
#' @export
read_mt3dms_array <- function(...) {
  .Deprecated(new = "rmti_parse_array", old = "read_mt3dms_array")
  rmti_parse_array(...)
}

#' Read mt3dms variables
#' If all are numbers, returns numeric, otherwise returns character vector
rmti_parse_variables <- function(remaining_lines) {
  return(RMODFLOW:::read_modflow_variables(remaining_lines))
}

#' @describeIn rmti_parse_variables Deprecated function name
#' @export
read_mt3dms_variables <- function(...) {
  .Deprecated(new = "rmti_parse_variables", old = "read_mt3dms_variables")
  rmti_parse_variables(...)
}


#' Remove empty elements from a vector of strings.
#' @param vector_of_strings Vector of strings.
#' @return Vector of strings without the empty items.
rmti_remove_empty_strings <- function(vector_of_strings) {
  return(vector_of_strings[which(vector_of_strings!='')])
}

#' @describeIn rmti_remove_empty_strings Deprecated function name
#' @export
remove_empty_strings <- function(...) {
  .Deprecated(new = "rmti_remove_empty_strings", old = "remove_empty_strings")
  rmti_remove_empty_strings(...)
}
