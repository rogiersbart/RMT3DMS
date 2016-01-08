#' Plot a layer of a MT3DMS 3D array
#' 
#' \code{plot.rmt3dms_3d_array} plots a 2D section through a MT3DMS 3D array.
#' 
#' @param rmt3dms_3d_array an object of class rmt3dms_3d_array, or a 3D array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param btn basic transport file object
#' @param mask a 3D array with 0 or F indicating inactive cells; defaults to btn$icbund
#' @param ... arguments provided to RMODFLOW::plot.rmodflow_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method plot rmt3dms_3d_array
#' @export
plot.rmt3dms_3d_array <- function(rmt3dms_3d_array,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  btn,
                                  mask = btn$icbund,
                                  ...) {
  dis <- convert_btn_to_dis(btn)
  plot(RMODFLOW::create_rmodflow_array(rmt3dms_3d_array), dis = dis, mask = mask, i = i, j = j, k = k, ...)
}
