#' Plot a MODFLOW 2D array
#' 
#' \code{plot.mt3dms_2d_array} plots a MODFLOW 2D array.
#' 
#' @param mt3dms_2d_array an object of class mt3dms_2d_array, or a 2D matrix
#' @param btn basic transport file object
#' @param mask a 2D array with 0 or F indicating inactive cells; defaults to the first layer of btn$ICBUND
#' @param colour_palette a colour palette for imaging the array values
#' @param zlim vector of minimum and maximum value for the colour scale
#' @param levels levels to indicate on the colour scale; defaults to nlevels pretty breakpoints
#' @param nlevels number of levels for the colour scale; defaults to 7
#' @param type plot type: 'fill' (default), 'grid' or 'contour'
#' @param add logical; if TRUE, provide ggplot2 layers instead of object, or add 3D plot to existing rgl device; defaults to FALSE
#' @param height_exaggeration height exaggeration for 3D plot; optional
#' @param binwidth binwidth for contour plot; defaults to 1/20 of zlim
#' @param label logical; should labels be added to contour plot
#' @param prj projection file object
#' @param target_CRS coordinate reference system for the plot
#' @param alpha transparency value; defaults to 1
#' @param plot3d logical; should a 3D plot be made
#' @param height 2D array for specifying the 3D plot z coordinate
#' @return ggplot2 object or layer; if plot3D is TRUE, nothing is returned and the plot is made directly
#' @method autoplot mt3dms_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl RTOOLZ
autoplot.mt3dms_2d_array <- function(mt3dms_2d_array, btn, mask=btn$ICBUND[,,1], colour_palette=rev_rainbow, zlim = range(mt3dms_2d_array[which(mask!=0)], finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 7, type='fill', add=FALSE,plot3d=FALSE,height_exaggeration=100,binwidth=1,label=TRUE,prj=NULL,target_CRS=NULL,alpha=1,height=NULL)
{
  dis <- convert_btn_to_dis(btn)
  plot(as.modflow_2d_array(mt3dms_2d_array),dis=dis,mask=mask,colour_palette=colour_palette,zlim=zlim,levels=levels,nlevels=nlevels,type=type,add=add,plot3d=plot3d,height_exaggeration=height_exaggeration,bindwidth=binwidth,label=label,prj=prj,target_CRS=target_CRS,alpha=alpha,height=height)
}