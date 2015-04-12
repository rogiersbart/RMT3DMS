#' Convert real world coordinates to mt3dms coordinates
#' 
#' @export
convert_real_to_btn <- function(x,y,prj,z=NULL,btn=NULL)
{
  x <- x-prj$origin[1]
  y <- y-prj$origin[2]
  angle <- atan(y/x)*180/pi - prj$rotation
  angle[which(is.na(angle))] <- 90-prj$rotation
  s <- sqrt(x^2+y^2)
  x <- cos(angle*pi/180)*s
  y <- sin(angle*pi/180)*s
  if(!is.null(z)) z <- z - prj$origin[3]
  dat <- data.frame(x=x,y=y)
  if(!is.null(z)) dat$z <- z
  if(!is.null(btn))
  {
    if(ncol(dat)==3)
    {
      btn$BOTM <- btn$TOP <- btn$DZ
      btn$BOTM[,,1] <- btn$HTOP - btn$DZ[,,1]
      btn$TOP[,,1] <- btn$HTOP
      for(k in 2:btn$NLAY)
      {
        btn$BOTM[,,k] <- btn$BOTM[,,(k-1)] - btn$DZ[,,k]
        btn$TOP[,,k] <- btn$BOTM[,,(k-1)]
      }
    }
    for(i in 1:nrow(dat))
    {
      dat$i[i] <- which(cumsum(btn$DELC) > sum(btn$DELC)-dat$y[i])[1]
      dat$j[i] <- which(cumsum(btn$DELR) > dat$x[i])[1]
      
      dat$roff[i] <- (sum(btn$DELC)-dat$y[i] -(cumsum(btn$DELC) - btn$DELC/2)[dat$i[i]])/btn$DELC[dat$i[i]]
      dat$coff[i] <- (dat$x[i] -(cumsum(btn$DELR) - btn$DELR/2)[dat$j[i]])/btn$DELR[dat$j[i]]
      if(!is.null(z)) 
      {
        dat$k[i] <- which(btn$BOTM[dat$i[i],dat$j[i],] < dat$z[i])[1]
        dat$loff[i] <- -(dat$z[i]-(btn$TOP[dat$i[i],dat$j[i],dat$k[i]]+btn$BOTM[dat$i[i],dat$j[i],dat$k[i]])/2)/btn$DZ[dat$i[i],dat$j[i],dat$k[i]]
      }
    }
  }
  return(dat)
}