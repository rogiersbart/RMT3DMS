#' Add rmt3dms array class to object based on object dimensions
#' 
#' @param obj object to add class to
#' @export
create_rmt3dms_array <- function(obj = NA, dim = NULL) {
  if(!is.null(dim)) obj <- array(obj, dim = dim)
  if(length(dim(obj))==2) {
    class(obj) <- 'rmt3dms_2d_array'
  } else if(length(dim(obj))==3) {
    class(obj) <- 'rmt3dms_3d_array'
  } else if(length(dim(obj))==4) {
    class(obj) <- 'rmt3dms_4d_array'
  } else {
    stop('Please provide 2d matrix, or 2d, 3d or 4d array.')
  }
  return(obj)
}
