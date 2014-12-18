#' Provide cell centers
#' 
#' @export
coord_cell_centers <- function(btn)
{
  cell_centers <- btn$DZ*NA
  cell_centers[,,1] <- btn$HTOP-btn$DZ[,,1]/2
  for(k in 2:btn$NLAY)
  {
    cell_centers[,,k] <- cell_centers[,,(k-1)] - btn$DZ[,,(k-1)]/2 - btn$DZ[,,k]/2
  }
  return(cell_centers)
}