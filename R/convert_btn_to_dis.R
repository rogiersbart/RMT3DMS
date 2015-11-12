#' Convert RMT3DMS btn to RMODFLOW dis object
#' 
#' @param btn btn object
#' @return dis object
#' @export
convert_btn_to_dis <- function(btn)
{
  dis <- NULL
  dis$NLAY <- btn$NLAY
  dis$NROW <- btn$NROW
  dis$NCOL <- btn$NCOL
  dis$NPER <- btn$NPER
  dis$ITMUNI <- btn$TUNIT
  dis$LENUNI <- btn$LUNIT
  dis$LAYCBD <- rep(0,dis$NLAY)
  dis$DELR <- btn$DELR
  dis$DELC <- btn$DELC
  dis$TOP <- btn$HTOP
  dis$BOTM <- btn$DZ*NA
  dis$BOTM[,,1] <- btn$HTOP - btn$DZ[,,1]
  if(dis$NLAY >1) for(k in 2:dis$NLAY) dis$BOTM[,,k] <- dis$BOTM[,,k-1]-btn$DZ[,,k]
  dis$PERLEN <- btn$PERLEN
  dis$NSTP <- btn$NSTP
  dis$TSMULT <- btn$TSMULT
  dis$SSTR <- NA
  class(dis) <- c('dis','modflow_package')
  return(dis)
}