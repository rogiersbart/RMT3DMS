#' Add class to object
#' 
#' @export
as.mt3dms_2d_array <- function(obj)
{
  class(obj) <- 'mt3dms_2d_array'
  return(obj)
}