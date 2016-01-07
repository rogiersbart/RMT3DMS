#' Get cell center z coordinates from a btn object
#' 
#' @param btn btn object
#' @return 3d array with cell center z coordinates
#' 
#' @rdname cell_centers
#' @method cell_centers btn
#' @export
cell_centers.btn <- function(btn)
{
  cell_centers <- btn$dz*NA
  cell_centers[,,1] <- btn$htop-btn$dz[,,1]/2
  for(k in 2:btn$nlay)
  {
    cell_centers[,,k] <- cell_centers[,,(k-1)] - btn$dz[,,(k-1)]/2 - btn$dz[,,k]/2
  }
  return(cell_centers)
}
