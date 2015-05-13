#' Plot a layer of a MT3DMS 3D array
#' 
#' \code{plot.mt3dms_3d_array} plots a 2D section through a MT3DMS 3D array.
#' 
#' @param mt3dms_3d_array An object of class mt3dms_3d_array, or a 3D array
#' @param mask A 3D icbund array with 1 or TRUE indicating active cells, and 0 or F indicating inactive cells
#' @param layer The number of the layer to plot
#' @param color.palette A color palette for imaging the parameter values
#' @param zlim
#' @param levels
#' @param nlevels
#' @param main
#' @return None
#' @method plot mt3dms_3d_array
#' @export
plot.mt3dms_3d_array <- function(mt3dms_3d_array, i=NULL, j=NULL, k=NULL, btn, mask=btn$ICBUND, zlim = range(mt3dms_3d_array[ifelse0(is.null(i),c(1:dim(mt3dms_3d_array)[1]),i),ifelse0(is.null(j),c(1:dim(mt3dms_3d_array)[2]),j),ifelse0(is.null(k),c(1:dim(mt3dms_3d_array)[3]),k)], finite=TRUE), color.palette=rev_rainbow, nlevels = 7, ...)
{
#   mt3dms_2d_array <- mt3dms_3d_array[,,k]
#   class(mt3dms_2d_array) <- 'mt3dms_2d_array'
#   mask <- mask[,,k]
#   plot(mt3dms_2d_array, btn, mask=mask, zlim=zlim, ...)
  
  if(!is.null(k))
  {
    mt3dms_2d_array <- mt3dms_3d_array[,,k]
    class(mt3dms_2d_array) <- 'mt3dms_2d_array'
    mask <- mask[,,k]
    plot(mt3dms_2d_array, btn, mask=mask, zlim=zlim, ...)
  } else {
    xy <- NULL
    xy$x <- cumsum(btn$DELR)-btn$DELR/2
    xy$y <- (cumsum(btn$DELC)-btn$DELC/2)
    mask[which(mask==0)] <- NA
    btn$CENTER <- cell_centers(btn)
    if(is.null(i) & !is.null(j))
    {
      ids <- factor(1:(btn$NROW*btn$NLAY))
      xWidth <- rep(btn$DELC,btn$NLAY)
      yWidth <- btn$DZ[,j,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$y,each=4),y=rep(btn$CENTER[,j,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((mt3dms_3d_array[,j,]*mask[,j,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
    } else if(!is.null(i) & is.null(j))
    {
      ids <- factor(1:(btn$NCOL*btn$NLAY))
      xWidth <- rep(btn$DELR,btn$NLAY)
      yWidth <- btn$DZ[i,,]
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(btn$CENTER[i,,],each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c((mt3dms_3d_array[i,,]*mask[i,,]^2)))
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      return(ggplot(datapoly, aes(x=x, y=y)) +
               geom_polygon(aes(fill=value, group=id)) +
               scale_fill_gradientn(colours=color.palette(nlevels),limits=zlim))
    }
  }
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