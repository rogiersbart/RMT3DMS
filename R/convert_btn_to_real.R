#' Convert mt3dms coordinates to real world coordinates
#' 
#' @param x mt3dms x coordinate
#' @param y mt3dms y coordinate
#' @param z mt3dms z coordinate
#' @param i mt3dms row number
#' @param j mt3dms column number
#' @param k mt3dms layer number
#' @param prj prj object
#' @param btn btn object
#' @details Provide either xyz or ijk
#' @return data frame with real world x and y coordinates
#' @export
convert_btn_to_real <- function(x=NULL,y=NULL,z=NULL,i=NULL,j=NULL,k=NULL,prj,btn=NULL)
{
  convert_dis_to_real(x=x,y=y,z=z,i=i,j=j,k=k,prj=prj,dis=btn)
}
