#' Add class to object
#' 
#' @export
as.mt3dms_3d_array <- function(obj)
{
  class(obj) <- 'mt3dms_3d_array'
  return(obj)
}