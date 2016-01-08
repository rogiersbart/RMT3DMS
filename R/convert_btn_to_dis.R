#' Convert RMT3DMS btn to RMODFLOW dis object
#' 
#' @param btn btn object
#' @return dis object
#' @export
convert_btn_to_dis <- function(btn) {
  dis <- NULL
  dis$nlay <- btn$nlay
  dis$nrow <- btn$nrow
  dis$ncol <- btn$ncol
  dis$nper <- btn$nper
  dis$itmuni <- btn$tunit
  dis$lenuni <- btn$lunit
  dis$laycbd <- rep(0,dis$nlay)
  dis$delr <- btn$delr
  dis$delc <- btn$delc
  dis$top <- btn$htop
  dis$botm <- btn$dz*NA
  dis$botm[,,1] <- btn$htop - btn$dz[,,1]
  if(dis$nlay >1) for(k in 2:dis$nlay) dis$botm[,,k] <- dis$botm[,,k-1]-btn$dz[,,k]
  dis$perlen <- btn$perlen
  dis$nstp <- btn$nstp
  dis$tsmult <- btn$tsmult
  dis$sstr <- NA
  class(dis) <- c('dis','modflow_package')
  return(dis)
}
