#' Plot a MODFLOW 2D array
#' 
#' \code{plot.rmt3dms_2d_array} plots a MODFLOW 2D array.
#' 
#' @param rmt3dms_2d_array an object of class rmt3dms_2d_array, or a 2D matrix
#' @param btn basic transport file object
#' @param ... arguments passed to plot.rmodflow_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method plot rmt3dms_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl quadprog
plot.rmt3dms_2d_array <- function(rmt3dms_2d_array,
                                  btn,
                                  mask = {warning('Using first icbund layer as mask.', call. = FalSE);btn$icbund[,,1]},
                                  ...) {
  dis <- convert_btn_to_dis(btn)
  plot(create_rmodflow_array(rmt3dms_2d_array), dis=dis, mask = mask, ...)
}
