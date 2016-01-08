#' Add rmt3dms array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @param dim the dim attribute for the array to be created; by default, dim(obj) is used
#' @export
create_rmt3dms_array <- function(obj = NA, dim = NULL) {
  array <- RMODFLOW::create_rmodflow_array(obj = obj, dim = dim)
  class(array) <- convert_rmodflow_to_rmt3dms_class(array)
  return(array)
}
