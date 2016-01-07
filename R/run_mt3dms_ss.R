#' Run an MT3DMS model
#' 
#' \code{run_mt3dms_ss} keeps on running an MT3DMS model until convergence is reached
#' 
#' @param file path to name file; typically '*.nam'
#' @param threshold threshold giving the maximum concentration difference for two consecutive simulations
#' @param report logical, should information be printed after each iteration
#' @param report_plot logical, should information be plotted after each iteration
#' @param maxit maximum number of iterations
#' @param ss ss object, to continue a simulation from a previous call to \code{run_mt3dms_ss}
#' @param ... parameters passed to \code{run_mt3dms}
#' @export
run_mt3dms_ss <- function(file,threshold,report=TRUE,report_plot=TRUE,maxit=100,ss=NULL,...)
{
  dir <- dirname(file)
  file <- basename(file)
  if(is.null(ss)) ss <- data.frame(run=0,time=0,max_conc_diff=NA)
  attr(ss,'threshold') <- threshold
  class(ss) <- c('ss','data.frame')
  convergence <- FALSE
  it <- 0
  while(!convergence)
  {
    run_mt3dms(paste0(dir,'/',file),...)
    nam <- read_nam(paste0(dir,'/',file))
    btn <- read_btn(paste0(dir,'/',nam$Fname[which(nam$Ftype=='BTN')]))
    if(201 %in% nam$Nunit) 
    {
      ucn <- read_ucn(paste0(dir,'/',nam$Fname[which(nam$Nunit==201)]),btn=btn)
    } else {
      ucn <- read_ucn(paste0(dir,'/MT3D001S.UCN'),btn=btn)  
    }
    ss <- rbind(ss,c(tail(ss[,1],1)+1,tail(ss[,2],1)+btn$perlen,max(btn$sconc[[1]]-ucn$CNEW[[1]],na.rm=TRUE)))
    if(tail(ss[,3],1) <= threshold) convergence <- TRUE
    if(report) print(tail(ss,1),row.names=FALSE)
    if(report_plot) print(plot(ss))
    if(!convergence)
    {
      btn$sconc[[1]] <- ucn$CNEW[[1]]
      write_btn(btn,file=paste0(dir,'/',nam$Fname[which(nam$Ftype=='BTN')]))
    }
    it <- it+1
    if(it >= maxit) break
  }
  return(ss)
}
