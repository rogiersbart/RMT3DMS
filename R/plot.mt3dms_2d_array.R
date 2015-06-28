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
#' @method plot mt3dms_2d_array
#' @export
#' @import ggplot2 directlabels akima rgl RTOOLZ
plot.mt3dms_2d_array <- function(mt3dms_2d_array, btn, mask=btn$ICBUND[,,1], colour_palette=rev_rainbow, zlim = range(mt3dms_2d_array[which(mask!=0)], finite=TRUE), levels = pretty(zlim, nlevels), nlevels = 7, type='fill', add=FALSE,plot3d=FALSE,height_exaggeration=100,binwidth=1,label=TRUE,prj=NULL,target_CRS=NULL,alpha=1,height=NULL)
{
  if(plot3d)
  {
    x <- (cumsum(btn$DELR)-btn$DELR/2)
    y <- sum(btn$DELC) - (cumsum(btn$DELC)-btn$DELC/2)
    z <- t(height)*height_exaggeration
    if(!add) open3d()
    colorlut <- colour_palette(nlevels) # height color lookup table
    col <- colorlut[round(approx(seq(zlim[1],zlim[2],length=nlevels+1),seq(0.5,nlevels+0.5,length=nlevels+1),xout=c(z/height_exaggeration),rule=2)$y) ] # assign colors to heights for each point
    alpha <- rep(1,length(col))
    alpha[which(c(t(mask))==0)] <- 0
    if(type=='fill') surface3d(x,y,z,color=col,alpha=alpha,back='lines',smooth=FALSE) 
    if(type=='grid') surface3d(x,y,z,front='lines',alpha=alpha,back='lines',smooth=FALSE) 
  } else
  {
    xy <- expand.grid(cumsum(btn$DELR)-btn$DELR/2,sum(btn$DELC)-(cumsum(btn$DELC)-btn$DELC/2))
    names(xy) <- c('x','y')
    mask[which(mask==0)] <- NA
    
    if(type=='fill')
    {  
      ids <- factor(1:(btn$NROW*btn$NCOL))
      xWidth <- rep(btn$DELR,btn$NROW)
      yWidth <- rep(btn$DELC,each=btn$NCOL)
      positions <- data.frame(id = rep(ids, each=4),x=rep(xy$x,each=4),y=rep(xy$y,each=4))
      positions$x[(seq(1,nrow(positions),4))] <- positions$x[(seq(1,nrow(positions),4))] - xWidth/2
      positions$x[(seq(2,nrow(positions),4))] <- positions$x[(seq(2,nrow(positions),4))] - xWidth/2
      positions$x[(seq(3,nrow(positions),4))] <- positions$x[(seq(3,nrow(positions),4))] + xWidth/2
      positions$x[(seq(4,nrow(positions),4))] <- positions$x[(seq(4,nrow(positions),4))] + xWidth/2
      positions$y[(seq(1,nrow(positions),4))] <- positions$y[(seq(1,nrow(positions),4))] - yWidth/2
      positions$y[(seq(2,nrow(positions),4))] <- positions$y[(seq(2,nrow(positions),4))] + yWidth/2
      positions$y[(seq(3,nrow(positions),4))] <- positions$y[(seq(3,nrow(positions),4))] + yWidth/2
      positions$y[(seq(4,nrow(positions),4))] <- positions$y[(seq(4,nrow(positions),4))] - yWidth/2
      values <- data.frame(id = ids,value = c(t(mt3dms_2d_array*mask^2)))
      if(!is.null(prj))
      {
        new_positions <- convert_btn_to_real(x=positions$x,y=positions$y,prj)
        positions$x <- new_positions$x
        positions$y <- new_positions$y
      }
      if(!is.null(target_CRS))
      {
        new_positions <- addosmmerc(data.frame(x=positions$x,y=positions$y),CRS_from=CRS(prj$projection),CRS_to=target_CRS)
        positions$x <- new_positions$Xosmmerc
        positions$y <- new_positions$Yosmmerc                    
      }
      datapoly <- merge(values, positions, by=c("id"))
      datapoly <- na.omit(datapoly)
      if(add)
      {
        return(geom_polygon(aes(x=x,y=y,fill=value, group=id),data=datapoly,alpha=alpha)) # +
          #scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim))
      } else {
        return(ggplot(datapoly, aes(x=x, y=y)) +
          geom_polygon(aes(fill=value, group=id),alpha=alpha) +
          scale_fill_gradientn(colours=colour_palette(nlevels),limits=zlim))
      }
    }
    if(type=='contour')
    {
      xy$z <- c(t(mt3dms_2d_array*mask^2))
      xyBackup <- xy
      xy <- na.omit(xy)
      xy <- interp(xy$x,xy$y,xy$z,xo=seq(min(xy$x),max(xy$x),length=ceiling(sum(btn$DELR)/min(btn$DELR))),yo=seq(min(xy$y),sum(max(xy$y)),length=ceiling(sum(btn$DELC)/min(btn$DELC))))
      xy$x <- rep(xy$x,ceiling(sum(btn$DELC)/min(btn$DELC)))
      xy$y <- rep(xy$y,each=ceiling(sum(btn$DELR)/min(btn$DELR)))
      xy$z <- c(xy$z)
      xy <- as.data.frame(xy)
      xy <- xy[which(xy$z >= zlim[1] & xy$z <= zlim[2]),]
      closestGridPoints <- apply(xy[,c('x','y')],1,function(x) which.min((x[1]-xyBackup$x)^2 + (x[2]-xyBackup$y)^2))
      xy$z[which(is.na(xyBackup$z[closestGridPoints]))] <- NA
      rm(xyBackup)
      if(add)
      {
        if(label) return(stat_contour(aes(x=x,y=y,z=z,colour = ..level..),data=xy,binwidth=binwidth))
        if(!label) return(stat_contour(aes(x=x,y=y,z=z),colour='black',data=xy,binwidth=binwidth))
      } else {
        if(label) return(direct.label(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(aes(colour = ..level..),binwidth=binwidth)))
        if(!label) return(ggplot(xy, aes(x=x, y=y, z=z)) + stat_contour(colour = 'black',binwidth=binwidth))
      }
    }
  }
}