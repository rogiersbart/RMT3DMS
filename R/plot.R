#' Generic function for static 2D plotting
#' 
#' @rdname rmt_plot
#' @export
rmt_plot <- function(...) {
  UseMethod('rmt_plot')
}

#' Plot an MT3DMS ss object
#' 
#' \code{rmt_plot.ss} plots an RMT3DMS ss object
#' 
#' @param ss ss object
#' @method rmt_plot ss
#' @export
rmt_plot.ss <- function(ss) {
  ggplot2::ggplot(ss,ggplot2::aes(x=time,y=max_conc_diff))+
    ggplot2::geom_point()+
    ggplot2::geom_path()+
    ggplot2::geom_hline(yintercept=attr(ss,'threshold'),colour='gray',linetype='dashed')
}

#' Plot a MODFLOW 2D array
#' 
#' \code{rmt_plot.rmt3dms_2d_array} plots a MODFLOW 2D array.
#' 
#' @param rmt_2d_array an object of class rmt_2d_array
#' @param btn basic transport file object
#' @param ... arguments passed to rmf_plot.rmf_2d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmt_plot rmt_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl quadprog
rmt_plot.rmt_2d_array <- function(rmt_2d_array,
                                  btn,
                                  mask = {warning('Using first icbund layer as mask.', call. = FALSE);btn$icbund[,,1]},
                                  ...) {
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(RMODFLOW::rmf_create_array(rmt_2d_array), dis=dis, mask = mask, ...)
}

#' Plot a layer of a MT3DMS 3D array
#' 
#' \code{rmt_plot.rmt_3d_array} plots a 2D section through a MT3DMS 3D array.
#' 
#' @param rmt_3d_array an object of class rmt_3d_array
#' @param i row number to plot
#' @param j column number to plot
#' @param k layer number to plot
#' @param btn basic transport file object
#' @param mask a 3D array with 0 or F indicating inactive cells; defaults to btn$icbund
#' @param ... arguments provided to RMODFLOW::rmf_plot.rmf_3d_array
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method rmt_plot rmt_3d_array
#' @export
rmt_plot.rmt_3d_array <- function(rmt_3d_array,
                                  i = NULL,
                                  j = NULL,
                                  k = NULL,
                                  btn,
                                  mask = btn$icbund,
                                  ...) {
  dis <- rmt_convert_btn_to_dis(btn)
  RMODFLOW::rmf_plot(RMODFLOW::rmf_create_array(rmt_3d_array), dis = dis, mask = mask, i = i, j = j, k = k, ...)
}

