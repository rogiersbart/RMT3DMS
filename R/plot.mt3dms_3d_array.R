#' Plot a layer of a MODFLOW 3D array
#' 
#' \code{plot.mt3dms_3d_array} plots a layer of a MODFLOW 3D array.
#' 
#' @param mt3dms_3d_array An object of class mt3dms_3d_array, or a 3D array
#' @param ibound A 3D ibound array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param layer The number of the layer to plot
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot mt3dms_3d_array
#' @export
plot.mt3dms_3d_array <- function(mt3dms_3d_array, layer=1, btn, ibound=mt3dms_3d_array*0+1, zlim = range(mt3dms_3d_array[,,layer][which(ibound!=0)], finite=TRUE), ...)
{
  mt3dms_2d_array <- mt3dms_3d_array[,,layer]
  class(mt3dms_2d_array) <- 'mt3dms_2d_array'
  ibound <- ibound[,,layer]
  plot(mt3dms_2d_array, btn, ibound=ibound, zlim=zlim, ...)
  
  
#   if(layer=='all')
#   {
#     plot3d.mt3dms_2d_array(mt3dms_3d_array[,,1])
#     for(k in 2:dim(mt3dms_3d_array)[3])
#     {
#       plot3d.mt3dms_2d_array(mt3dms_3d_array[,,1],add=TRUE)
#     }
#     
#   } else {
#     plot3d.mt3dms_2d_array(mt3dms_3d_array[,,layer])
#   }
}