#' Add mt3dms_2d_array class to object
#' 
#' @param obj object to add class to
#' @export
as.mt3dms_2d_array <- function(obj)
{
  class(obj) <- 'mt3dms_2d_array'
  return(obj)
}