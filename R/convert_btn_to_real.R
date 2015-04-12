#' Convert mt3dms coordinates to real world coordinates
#' 
#' @export
convert_btn_to_real <- function(x,y,prj,z=NULL)
{
  angle <- atan(y/x)*180/pi+prj$rotation
  angle[which(is.na(angle))] <- prj$rotation + 90
  s <- sqrt(x^2+y^2)
  x <- prj$origin[1]+ cos(angle*pi/180)*s
  y <- prj$origin[2]+ sin(angle*pi/180)*s
  if(!is.null(z)) z <- prj$origin[3]+z
  ifelse(!is.null(z),return(data.frame(x=x,y=y,z=z)),return(data.frame(x=x,y=y)))
}